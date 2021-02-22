open Unix
open Node
open Command
open Miscellaneous
open Block
open Mutex

(* L'adresse IP et le port du mineur courant *)
let my_ip = ref "127.0.0.1"
let my_port = ref 8000
let set_my_ip ip = my_ip := ip
let set_my_port port = my_port := port

let exit_miner = ref false

(* La représentation du mineur courant *)
let me = ref { 
  lazy_part = {
    id = 1;
    accounts = [];
    my_internet_adress = (inet_addr_of_string "127.0.0.1", 8000);
    fullnode_info = None;
    connected_account = None
  };
  blockchain = [];
  dns = DNS.empty;
  current_coinbase = 50.0
  }

let mutex_new_me = create()

let get_all_accounts_adress () =
  List.fold_left (fun adress (account: account) ->
    account.adress :: adress) [] !me.lazy_part.accounts 

let get_my_connected_info () =
  let ip, port = !me.lazy_part.my_internet_adress in
  (
    ip,
    port,
    get_all_accounts_adress(),
    !me.dns
  )

let update_me new_me =
  lock mutex_new_me;
  me := new_me;
  unlock mutex_new_me

let get_free_id dns id =
  if DNS.is_empty dns then
    id + 1
  else
    let rec aux acc =
      let test1 = (DNS.exists (fun dns_t -> dns_t.id = acc) dns) in
      let test2 = id = acc in
      if test1 || test2 then
        aux (acc + 1)
      else
        acc in
    aux 1

let string_of_me () =
  let my_ip, my_port = !me.lazy_part.my_internet_adress in

  
  string_of_int !me.lazy_part.id ^ "#" ^ string_of_inet_addr my_ip ^ ":" ^ string_of_int my_port


let broadcast_miner set_miner f init_message =
  (* Problème si un mineur s'est déco et qu'il est toujours dans la liste -> rattraper l'exception ECONNREFUSED *)
  let dns_list = Node.DNS.elements set_miner in
  let rec iter_list (miner_list: Node.dns_translation list) message =
    match miner_list with
    |[] -> ()
    |miner :: next ->
      begin
        let ip, port = miner.internet_adress in

        (* On créé la socket puis on la connecte vers le mineur de la liste *)
        let s = socket PF_INET SOCK_STREAM 0 in
        setsockopt s SO_REUSEADDR true;
        Unix.connect s (ADDR_INET(ip, port));

        (* On prépare le message à envoyer. *)
        let out_chan = out_channel_of_descr s in

        let new_msg = f message in

        (* On envoie le message *)
        output_value out_chan new_msg;
        flush out_chan;

        (*On ferme la socket et on passe au mineur suivant *)
        Unix.shutdown s Unix.SHUTDOWN_ALL;

        (* On recommence avec le mineur suivant et le nouveau message *)
        iter_list next new_msg
      end in
  iter_list dns_list init_message


let send_msg_to_miner id dns msg =
  let miner =  DNS.choose (DNS.filter (fun dns_t -> dns_t.id = id) dns) in

  let ip, port = miner.internet_adress in

  (* On créé la socket puis on la connecte vers le mineur de la liste *)
  let s = socket PF_INET SOCK_STREAM 0 in
  setsockopt s SO_REUSEADDR true;
  Unix.connect s (ADDR_INET(ip, port));

  (* On prépare le message à envoyer. *)
  let out_chan = out_channel_of_descr s in

  (* On envoie le message *)
  output_value out_chan msg;
  flush out_chan;

  (*On ferme la socket et on passe au mineur suivant *)
  Unix.shutdown s Unix.SHUTDOWN_ALL


let merge_and_return_new_dns dns_1 dns_2 =
  let last_dns_list = DNS.elements dns_1 in
  let rec aux l1 set_l2 new_id_list =
    match l1 with
    |[] -> set_l2, new_id_list
    |dns_t :: next ->
      let free_id = get_free_id set_l2 !me.lazy_part.id in
      aux next (DNS.add {dns_t with id = free_id} set_l2) ((dns_t.internet_adress, free_id) :: new_id_list) in
  aux last_dns_list dns_2 []


let create_account name =
  if not (List.exists (fun account -> account.account_name = name) !me.lazy_part.accounts) then
    begin
      let h_rng = Cryptokit.Random.hardware_rng () in
      let account_key = Cryptokit.RSA.new_key ~rng:h_rng 2048 in
      let new_account = {
        account_name = name;
        euc_balance = 0.0;
        rsa_key = account_key;
        adress = string_to_hexa (hash_of_public_key (get_public_key account_key))
      } in
      let new_me = {
        !me with
        lazy_part = {
          !me.lazy_part with
          accounts = new_account :: !me.lazy_part.accounts
        }
      } in
      update_me new_me;
      broadcast_miner !me.dns (fun m -> m) (New_account (!me.lazy_part.id, new_account.adress))
    end
  else
    begin
      print_string "Le nom de compte existe déjà";
      print_newline()
    end


let connect_account name =
  if (List.exists (fun account -> account.account_name = name) !me.lazy_part.accounts) then
    begin
      let acc = List.find (fun account -> account.account_name = name) !me.lazy_part.accounts in
      let new_me = {
        !me with
        lazy_part = {
          !me.lazy_part with
          connected_account = (Some acc.account_name) 
        }
      } in
      update_me new_me;
      print_string ("Vous êtes maintenant connecté au compte " ^ name);
      print_newline();
      print_string "Lancement du minage sur ce compte"
    end
  else
    begin
      print_string "Le nom de compte n'existe pas";
      print_newline();
    end


let disconnect_account () =
  let new_me = {
    !me with
    lazy_part = {
      !me.lazy_part with
      connected_account = None
    }
  } in
  update_me new_me

let merkel_of_tr_list trs =
  let hash_tr_list = List.map (fun tr -> string_to_hexa (sha3_of_string (string_of_transaction tr))) trs in
  make hash_tr_list


let create_generation_tr account =
  let input_gen = {
    previous_tr_hash = "";
    previous_out_index = -1;
    signature = "";
    public_key = get_public_key account.rsa_key
  } in

  let output_gen = {
    value = !me.current_coinbase;
    adress = account.adress
  } in

  let generation_tr = {
      inputs = [input_gen];
      outputs = [output_gen]
    } in
  let sign_gen = sign_transaction account.rsa_key generation_tr in
  let sign_input_gen = {
    input_gen with signature = sign_gen 
  } in
  {
    inputs = [sign_input_gen];
    outputs = [output_gen]
  }


let verif_blockchain blchain =
  let rec aux prev_hash chain =
    match chain with
    |[] -> true
    |bloc :: next ->
      begin
        if not (Cryptokit.string_equal prev_hash (hash_of_block_header bloc.block_h)) then
          false
        else
          aux bloc.block_h.previous_hash next
      end
        in
    aux (List.hd blchain).block_h.previous_hash (List.tl blchain)

let mutex_new_bloc = create()
let mutex_new_transaction = create()
let notify_new_block = ref false
let new_transaction_incoming = ref false

let (new_transaction: (transaction list ref)) = ref []


let create_header name_account new_nonce =
  let account = List.find (fun acc -> acc.account_name = name_account) !me.lazy_part.accounts in
  let prev_block = List.hd !me.blockchain in

  let hash_prev_block = sha3_of_string (string_of_block_header prev_block.block_h) in

  if List.length !new_transaction = 0 then
    let tr_gen = create_generation_tr account in
    let merkel_root = make [string_to_hexa (sha3_of_string (string_of_transaction tr_gen))] in
    {
      previous_hash = hash_prev_block;
      hash_merkelroot = hash_root merkel_root; 
      timestamp = Unix.time(); 
      target = prev_block.block_h.target; 
      nonce = new_nonce
    }
  else
    let merkel_root = hash_root (make (List.map (fun tr -> string_to_hexa (sha3_of_string (string_of_transaction tr))) !new_transaction)) in
    {
      previous_hash = hash_prev_block;
      hash_merkelroot = merkel_root;
      timestamp = Unix.time();
      target = prev_block.block_h.target;
      nonce = new_nonce
    }


(*let rec hashcash_proof_of_work block =

  if !notify_new_block then
    begin
    print_string "un nouveau bloc a été ajouté";
    print_newline();
    let new_block = {
      block with
      block_h = {
        block.block_h with
        previous_hash = hash_of_block_header (List.hd !me.blockchain)
      }
    } in
    print_string ("verif blockchain with new block : " ^  (string_of_bool (verif_blockchain (new_block ::!me.blockchain))));
    notify_new_block := false;
    let hash_header = hash_of_block_header new_block in
    if zint_of_hash hash_header < block.block_h.target then
      new_block
    else
      hashcash_proof_of_work {
        new_block with
        block_h = {
          new_block.block_h with
          timestamp = Unix.time();
          nonce = Z.add new_block.block_h.nonce (Z.of_int 1);
        }
      }
    end
  else
    begin
      let hash_header = hash_of_block_header block in
      if zint_of_hash hash_header < block.block_h.target then
        block
      else
        hashcash_proof_of_work {
          block with
          block_h = {
            block.block_h with
            timestamp = Unix.time();
            nonce = Z.add block.block_h.nonce (Z.of_int 1);
          }
        }
    end*)


let rec mine_block nonce =
  match !me.lazy_part.connected_account with
  |None -> mine_block Z.zero
  |Some name_account ->
    let new_header = create_header name_account nonce in
    let target = (List.hd !me.blockchain).block_h.target in

    let hash_h = hash_of_block_header new_header in
    lock mutex_new_bloc;
    if zint_of_hash hash_h < target && Cryptokit.string_equal new_header.previous_hash (hash_of_block_header (List.hd !me.blockchain).block_h) then
      begin
        print_string "un bloc a été trouvé";
        print_newline();

        let new_bloc = {
          block_h = new_header;
          transactions = !new_transaction
        } in

        print_string "On l'envoie a tous le monde";
        print_newline();
        broadcast_miner !me.dns (fun m -> m) (New_block (List.length !me.blockchain + 1, new_bloc));
        let new_me = {
          !me with
          blockchain = new_bloc :: !me.blockchain
        } in
        update_me new_me;

        print_newline();
        print_int (List.length !me.blockchain);
        print_newline();
        print_string ("condition blockchain : " ^ (string_of_bool (verif_blockchain !me.blockchain)));
        print_newline();

        flush_all();
        unlock mutex_new_bloc;
        mine_block Z.zero
      end
    else
      begin
        unlock mutex_new_bloc;
        mine_block (Z.add nonce Z.one)
      end

let verif_block block =
  let hash_header = hash_of_block_header block in
  let prev_target = List.hd !me.blockchain in
  zint_of_hash hash_header < prev_target.block_h.target
  