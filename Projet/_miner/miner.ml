open Unix
open Node
open Command
open Miscellaneous
open Block
open Mutex

(* L'adresse IP et le port du mineur courant *)
let my_ip = ref ""
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
        try
          if miner.node_type = Full then
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

              Unix.shutdown s Unix.SHUTDOWN_ALL;
              close s;

              (* On recommence avec le mineur suivant et le nouveau message *)
              iter_list next new_msg
            end
        with
        |Unix_error (error, msg1, msg2) ->
          print_string (error_message error);
          print_newline();
          print_string "erreur de broadcast\n"
      end in
  iter_list dns_list init_message


let send_msg_to_miner id dns msg =
  try
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

    Unix.shutdown s Unix.SHUTDOWN_ALL;
    close s
  with
  |Not_found ->
    print_string "not found";
    ()



let merge_and_return_new_dns dns_1 dns_2 =
  let union_dns = DNS.union dns_1 dns_2 in
  DNS.iter (fun dns_t -> print_string (string_of_dns_t dns_t); print_newline()) union_dns;
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
      update_me new_me
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
  update_me new_me;
  print_string "Le compte est deconnecté. Le mineur ne mine plus\n"

let merkel_of_tr_list trs =
  let hash_tr_list = List.map (fun tr -> string_to_hexa (sha3_of_string (string_of_transaction tr))) trs in
  make hash_tr_list


let create_generation_tr account =
  let h_rng = Cryptokit.Random.hardware_rng () in
  let rand_bytes = Bytes.create 40 in
  h_rng#random_bytes rand_bytes 0 39;
  let str_alea = Bytes.to_string rand_bytes in
  let input_gen = {
    previous_tr_hash = str_alea;
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


let get_account_by_name name_account = 
  List.find (fun acc -> acc.account_name = name_account) !me.lazy_part.accounts


let create_header name_account new_nonce =
  let account = get_account_by_name name_account in
  let prev_block = List.hd !me.blockchain in

  let hash_prev_block = sha3_of_string (string_of_block_header prev_block.block_h) in

  if List.length !new_transaction = 0 then
    begin
      let tr_gen = create_generation_tr account in
      let hash_gen_tr = sha3_of_string (string_of_transaction tr_gen) in
      let merkel_root = make [string_to_hexa (hash_gen_tr)] in
      new_transaction := tr_gen :: !new_transaction;
      {
        previous_hash = hash_prev_block;
        hash_merkelroot = hash_root merkel_root; 
        timestamp = Unix.time(); 
        target = prev_block.block_h.target; 
        nonce = new_nonce
      }
    end
  else
    let merkel_root = hash_root (make (List.map (fun tr -> string_to_hexa (sha3_of_string (string_of_transaction tr))) !new_transaction)) in
    {
      previous_hash = hash_prev_block;
      hash_merkelroot = merkel_root;
      timestamp = Unix.time();
      target = prev_block.block_h.target;
      nonce = new_nonce
    }
   




let rec mine_block nonce =
  match !me.lazy_part.connected_account with
  |None -> mine_block Z.zero
  |Some name_account ->

    lock mutex_new_transaction;
    lock mutex_new_bloc;
    let new_header = create_header name_account nonce in

    let target = (List.hd !me.blockchain).block_h.target in

    let hash_h = hash_of_block_header new_header in
    if zint_of_hash hash_h < target && Cryptokit.string_equal new_header.previous_hash (hash_of_block_header (List.hd !me.blockchain).block_h) then
      begin

        let new_bloc = {
          block_h = new_header;
          transactions = !new_transaction
        } in
        (*
        print_bloc new_bloc;
        print_string "------------------------------------------------";
        print_newline();*)
        broadcast_miner !me.dns (fun m -> m) (New_block (!me.lazy_part.id, List.length !me.blockchain + 1, new_bloc));
        let new_me = {
          !me with
          blockchain = new_bloc :: !me.blockchain
        } in
        update_me new_me;
        new_transaction := [];
        flush_all();
        unlock mutex_new_bloc;
        unlock mutex_new_transaction;
        mine_block Z.zero
      end
    else
      begin
        unlock mutex_new_bloc;
        unlock mutex_new_transaction;
        mine_block (Z.add nonce Z.one)
      end








let verif_block block =
  let hash_header = hash_of_block_header block in
  let hash_my_prev_block = sha3_of_string (string_of_block_header (List.hd !me.blockchain).block_h) in
  let prev_target = List.hd !me.blockchain in
  zint_of_hash hash_header < prev_target.block_h.target && String.equal block.previous_hash hash_my_prev_block

let stat_chain () =
  let last_bloc = List.hd !me.blockchain in
  let chain_lenght = List.length !me.blockchain in

  let rec mean_time_bloc sum_acc last_time chain =
    match chain with
    |[] -> sum_acc /. float_of_int chain_lenght
    |[genesis] -> sum_acc /. float_of_int chain_lenght
    |bloc :: next -> 
      let diff_time = last_time -. bloc.block_h.timestamp in
      mean_time_bloc (sum_acc +. diff_time) bloc.block_h.timestamp next in
  let time_last_bloc = Unix.time() -. last_bloc.block_h.timestamp in
  print_string ("statistique de la blockchain :");
  print_newline();
  print_string ("\tlongueur de la blockchain : " ^ string_of_int chain_lenght);
  print_newline();
  print_string ("\ttemps depuis le dernier bloc : " ^ string_of_float time_last_bloc);
  print_newline();
  print_string("\ttemps de minage moyen : " ^ string_of_float (mean_time_bloc 0. (List.hd !me.blockchain).block_h.timestamp !me.blockchain));
  print_newline();
  print_string ("\tIntégrité de la blockchain : " ^ (string_of_bool (verif_blockchain !me.blockchain)));
  print_newline()


(* Cette fonction permet de renvoyer les transactions et les outputs non référencé par une autre transactions et appartenant à mon compte *)
(*let get_real_output_account name_account =
  let current_account = get_account_by_name name_account in
  let a_adress = current_account.adress in

  let rec aux prev_hash_trs tr_output chain =
    match chain with
    |[] -> tr_output
    |bloc1 :: next_bl ->
      let trs1 = bloc1.transactions in

      let rec aux2 tr_list prev_hash acc =
        match tr_list with
        |[] ->  (prev_hash, acc)
        |tr :: next_tr ->
          begin
          let hash_tr = sha3_of_string (string_of_transaction tr) in
          if List.mem hash_tr prev_hash then
            begin
            let new_prev_hash = List.fold_left (fun prev_hash input -> input.previous_tr_hash :: prev_hash) prev_hash tr.inputs in
            aux2 next_tr new_prev_hash acc
            end
          else
            begin
            let new_prev_hash = List.fold_left (fun prev_hash input -> input.previous_tr_hash :: prev_hash) prev_hash tr.inputs in
            aux2 next_tr new_prev_hash (tr :: acc)
            end
          end in
      
      let new_prev_hash_trs, tr_out = aux2 trs1 prev_hash_trs tr_output in
      aux new_prev_hash_trs tr_out next_bl in
  
   let all_non_ref_tr = aux [] [] !me.blockchain in

   List.fold_left (
     fun real_tr_out tr ->
      List.fold_left (
        fun all_real_tr_out output ->
          if Cryptokit.string_equal output.adress a_adress then
            (tr, output) :: all_real_tr_out
          else
            all_real_tr_out
      ) real_tr_out tr.outputs
   ) [] all_non_ref_tr
*)


(* 
Cette fonction parcours la blockchain et renvoie
  -une liste de n-uplet. Chaque n-uplet n-uplets est :
      -la transaction associé au compte
      -la racine de l'arbre de merkel du bloc
      -l'ensemble des hash des transactions permettant de recrée la preuve
      -l'id du block pour vérifié si la transaction est confirmé dans la blockchain
  -une liste de n-uplet. Chaque n-uplet est
      -la transaction non référencé associé au compte
      -la racine de l'arbre de merkel du bloc
      -l'ensemble des hash des transactions permettant de recrée la preuve
      -l'id du block pour vérifié si la transaction est confirmé dans la blockchain  
  -l'ensemble des outputs non référencé du compte (la quantité de cryptomonnaie qu'il a actuellement)
*)
(*let get_tr_info account_adress account_key =

  let _, all_my_tr, all_my_out_tr, all_out_tr, _, _ = List.fold_left (fun (prev_hash, my_tr, tr_output, my_output, id_block, id_tr) block ->

    let merkel_root = block.block_h.hash_merkelroot in
    let merkel_tree_bloc = (make (List.map (fun tr -> string_to_hexa (sha3_of_string (string_of_transaction tr))) block.transactions)) in

    

    List.fold_left (fun (prev_hash, my_tr, tr_output, my_output, id_block, id_tr) current_tr ->
    
      let proof_tr = proof merkel_tree_bloc id_tr in

        let hash_tr = sha3_of_string (string_of_transaction current_tr) in
        
        let input_treatment (prev_hash, tr_input) input =
          (*print_newline();
          print_pk account_key;
          print_pk input.public_key;
          print_newline();*)
          if public_key_equal account_key input.public_key then
            begin
              (*print_int (List.length block.transactions);
              print_newline();
              print_transaction current_tr;
              print_newline();
              print_newline();*)
            (
              input.previous_tr_hash :: prev_hash,
              (current_tr, merkel_root, proof_tr, id_block) :: my_tr
            )
            end
          else
            begin
            (
              input.previous_tr_hash :: prev_hash,
              my_tr
            )
            end in


        let output_treatment (tr_output,my_output) output =
          if String.equal account_adress output.adress then
            (current_tr, merkel_root, proof_tr, id_block)::tr_output, output :: my_output
          else
            tr_output, my_output in

        if List.mem hash_tr prev_hash then
          let new_prev_hash, new_my_tr = List.fold_left input_treatment (prev_hash, my_tr) current_tr.inputs in
          (new_prev_hash, new_my_tr, tr_output, my_output, id_block + 1, id_tr + 1)
        else
          let new_prev_hash, new_my_tr = List.fold_left input_treatment (prev_hash, my_tr) current_tr.inputs in
          let my_new_tr_output, my_new_output = List.fold_left output_treatment (tr_output,my_output) current_tr.outputs in
          (new_prev_hash, new_my_tr, my_new_tr_output, my_new_output, id_block + 1, id_tr + 1)


      ) (prev_hash, my_tr, tr_output, my_output, id_block, 0) block.transactions) ([], [], [], [], 0, 0) (List.rev !me.blockchain) in
    
    

    (all_my_tr, all_my_out_tr, all_out_tr)*)




(* 
Cette fonction parcours la blockchain et renvoie
  -la liste des prev_hash de chaque transaction (non utile)
  -une liste de n-uplet. Chaque n-uplet n-uplets est :
      -la transaction associé au compte
      -la racine de l'arbre de merkel du bloc
      -l'ensemble des hash des transactions permettant de recrée la preuve
      -l'id du block pour vérifié si la transaction est confirmé dans la blockchain
  -une liste de n-uplet. Chaque n-uplet est
      -la transaction non rérérencé associé au compte
      -la racine de l'arbre de merkel du bloc
      -l'ensemble des hash des transactions permettant de recrée la preuve
      -l'id du block pour vérifié si la transaction est confirmé dans la blockchain  
  -l'ensemble des outputs non référencé du compte (la quantité de cryptomonnaie qu'il a actuellement)
  -un compteur pour les id de block (non utile)
  -un compteur pour les id de transaction (non utile)
*)
let get_tr_info account_adress account_key =

  List.fold_left (fun (prev_hash, my_tr, tr_output, my_output, id_block, id_tr) block ->

    let merkel_root = block.block_h.hash_merkelroot in
    let merkel_tree_bloc = (make (List.map (fun tr -> string_to_hexa (sha3_of_string (string_of_transaction tr))) block.transactions)) in

    List.fold_left (fun (prev_hash, my_tr, tr_output, my_output, id_block, id_tr) current_tr ->
    
      let proof_tr = proof merkel_tree_bloc id_tr in

        let hash_tr = sha3_of_string (string_of_transaction current_tr) in
        
        let input_treatment (prev_hash, tr_input) input =
          if public_key_equal account_key input.public_key then
            (
              input.previous_tr_hash :: prev_hash,
              (current_tr, merkel_root, proof_tr, id_block) :: my_tr
            )
          else
            (
              input.previous_tr_hash :: prev_hash,
              my_tr
            ) in


        let output_treatment my_output output =
          if String.equal account_adress output.adress then
            output :: my_output
          else
            my_output in

        if List.mem hash_tr prev_hash then
          let new_prev_hash, new_my_tr = List.fold_left input_treatment (prev_hash, my_tr) current_tr.inputs in
          (new_prev_hash, new_my_tr, tr_output, my_output, id_block + 1, id_tr + 1)
        else
          let new_prev_hash, new_my_tr = List.fold_left input_treatment (prev_hash, my_tr) current_tr.inputs in
          let my_new_output = List.fold_left output_treatment my_output current_tr.outputs in
          (new_prev_hash, new_my_tr, ((current_tr, merkel_root, proof_tr, id_block)::tr_output), my_new_output, id_block + 1, id_tr + 1)


      ) (prev_hash, my_tr, tr_output, my_output, id_block, 0) block.transactions) ([], [], [], [], 0, 0) (List.rev !me.blockchain)




let compute_account_balance name_account =
  let my_account = get_account_by_name name_account in
  let _, _, _, my_output_tr, _, _ = get_tr_info my_account.adress (get_public_key my_account.rsa_key) in
  List.fold_left (fun acc out -> acc +. out.value) 0.0 my_output_tr

let show_my_balance () =
  match !me.lazy_part.connected_account with
  |None -> ()
  |Some s ->
    print_string ("Solde du compte : " ^ string_of_float (compute_account_balance s));
    print_newline()

let create_transaction_for_miners adress value =
  match !me.lazy_part.connected_account with
  |None -> ()
  |Some s ->
    begin
      let my_account = get_account_by_name s in
      let _, _, all_my_tr, my_output_tr, _, _ = get_tr_info s (get_public_key my_account.rsa_key) in
      let account_balance = compute_account_balance s in


      if value > account_balance then
        print_string "Il n'y a pas assez d'argent sur le compte"
      else
        begin
          let rec aux tr_list acc_v acc_tr =
            match tr_list with
            |[] -> acc_tr, acc_v
            |(tr, _, _, _) :: next ->
              let balance_tr = List.fold_left (fun acc out -> if Cryptokit.string_equal my_account.adress out.adress then acc +. out.value else acc) 0.0 tr.outputs in
              if acc_v +. balance_tr >= value then
                tr :: acc_tr, acc_v +. balance_tr
              else
                aux next (acc_v +. balance_tr) (tr :: acc_tr) in
          
          let needed_tr, value_tr = aux all_my_tr 0.0 [] in
          let diff_value = value_tr -. value in
          
          
          let input_tr = List.map (fun tr ->
            let hash_tr = sha3_of_string (string_of_transaction tr) in 
            let (_, id) = List.fold_left (fun (test,id) out -> if Cryptokit.string_equal my_account.adress out.adress then (true, id) else (test, id + 1)) (false, 0) tr.outputs in
          {
            previous_tr_hash = hash_tr;
            previous_out_index = id;
            signature = "";
            public_key = get_public_key my_account.rsa_key
          }) needed_tr in

          let output_tr = {
            value;
            adress 
          } in

          let diff_output = {
            value = diff_value;
            adress = my_account.adress   
          } in
          let new_tr = if diff_value > 0.0 then {inputs = input_tr; outputs = [diff_output; output_tr]} else {inputs = input_tr; outputs = [output_tr]} in


          let signature = sign_transaction my_account.rsa_key new_tr in
          let new_tr = {
          new_tr with
          inputs = List.map (fun input -> {input with signature = signature}) new_tr.inputs
          } in
          lock mutex_new_transaction;
          new_transaction := new_tr :: !new_transaction;
          unlock mutex_new_transaction;
          broadcast_miner !me.dns (fun m -> m) (Send_transaction (Full, new_tr))
        end
      end

let gen_tr tr_info =
  let tr_split = String.split_on_char '>' tr_info in
  match tr_split with
  |[adress; value] -> create_transaction_for_miners adress (float_of_string value)
  |[]|_ -> print_string "error gen_tr\n"

let show_account_info a =
  print_string (string_of_account a);
  print_newline();
  print_string ("balance: " ^ string_of_float (compute_account_balance a.account_name));
  print_newline()

let show_connect_account () =
  match !me.lazy_part.connected_account with
  |None -> ()
  |Some s ->
    show_account_info (get_account_by_name s)

let show_all_account () =
  List.iter (fun a ->
    show_account_info a; print_newline()) !me.lazy_part.accounts

let compute_all_unique_adress () =
  let unique_adress_blockchain = List.fold_left (fun a_set block ->
    List.fold_left (fun a_set2 tr ->
      List.fold_left (fun a_set3 output ->
        AdressSet.add output.adress a_set3) a_set2 tr.outputs) a_set block.transactions) AdressSet.empty !me.blockchain in
  let my_unique_adress = List.fold_left (fun res_set (account: Node.account) -> AdressSet.add account.adress res_set) AdressSet.empty !me.lazy_part.accounts in
  (unique_adress_blockchain, my_unique_adress)

let show_all_adress () =
  let distant_adress, my_adress = compute_all_unique_adress() in
  print_string "Mes adresses :\n";
  AdressSet.iter (fun adress -> print_string ("\t" ^ adress); print_newline()) my_adress;
  print_string "Les adresses distante :\n";
  AdressSet.iter (fun adress -> print_string ("\t" ^ adress); print_newline()) distant_adress

let show_my_transaction option =
  let opt = String.split_on_char '#' option in
  match opt with
  |[nb_tr; show_gen] ->
    begin
      let nb_tr = int_of_string nb_tr in
      let show_gen = bool_of_string show_gen in
      match !me.lazy_part.connected_account with
      |None -> print_string "aucun compte connecté"
      |Some s ->
        let my_account = get_account_by_name s in
        let _, all_tr, tr_output, _, _, _ = get_tr_info my_account.adress (get_public_key my_account.rsa_key) in

        List.iteri (fun i (tr, merkel_root, proof_merkel, id_bloc_tr) ->
              if i < nb_tr then
                begin

                  let input_gen_test = (List.hd tr.inputs).previous_out_index in
                  if input_gen_test = -1 then
                    begin
                      if show_gen then
                        begin
                          print_string "transaction génératrice dans le bloc : ";print_int id_bloc_tr;print_string "  output:\n";
                          let hash_tr = string_to_hexa (sha3_of_string (string_of_transaction tr)) in
                          let list_proof_zint = List.map (fun x -> Z.of_string_base 16 x) proof_merkel in
                          let auth_tr = string_of_bool (authenticate hash_tr list_proof_zint merkel_root) in
                          List.iter (fun output ->
                                print_string "\tadress :";
                                print_string output.adress;
                                print_string "  balance_change: +";
                                print_float output.value;print_string " euc";
                                print_newline();
                            ) tr.outputs;
                          print_string "\tpreuve de la transaction :\n";
                          print_string "\t\tracine de merkel : "; print_string merkel_root;
                          print_newline();
                          print_string "\t\tpreuve de la transaction:\n";
                          List.iter (fun x -> print_string "\t\t\t";print_string x;print_newline()) proof_merkel;
                          print_string "\t\tauthentification: "; print_string auth_tr;
                          print_newline();
                        end
                    end
                    else
                      begin
                        print_string "transaction dans le bloc : ";print_int id_bloc_tr;print_string "  output:\n";
                        let hash_tr = string_to_hexa (sha3_of_string (string_of_transaction tr)) in
                        let list_proof_zint = List.map (fun x -> Z.of_string_base 16 x) proof_merkel in
                        let auth_tr = string_of_bool (authenticate hash_tr list_proof_zint merkel_root) in
                        List.iter (fun output ->
                          if String.equal my_account.adress output.adress then
                            begin
                              print_string "\tadress:";
                              print_string output.adress;
                              print_string "  balance_change: +";
                              print_float output.value;print_string " euc";
                              print_newline();
                            end
                            else
                              begin
                                print_string "\tadress:";
                                print_string output.adress;
                                print_string "  balance_change: -";
                                print_float output.value;print_string " euc";
                                print_newline();
                              end
                          ) tr.outputs;
                          print_string "\tpreuve de la transaction :\n";
                          print_string "\t\tracine de merkel : "; print_string merkel_root;
                          print_newline();
                          print_string "\t\tpreuve de la transaction:\n";
                          List.iter (fun x -> print_string "\t\t\t";print_string x;print_newline()) proof_merkel;
                          print_string "\t\tauthentification: "; print_string auth_tr;
                          print_newline();
                      end
                end
                ) all_tr;

                List.iteri (fun i (tr, merkel_root, proof_merkel, id_bloc_tr) ->
                  if i < nb_tr then
                    
                    let input_gen_test = (List.hd tr.inputs).previous_out_index in

                    let test = List.fold_left (fun test input -> if public_key_equal input.public_key (get_public_key my_account.rsa_key) then true else test ) false tr.inputs in

                    if input_gen_test != -1 && not test then
                      begin
                        print_string "transaction dans le bloc : ";print_int id_bloc_tr;print_string "  output:\n";
                        let hash_tr = string_to_hexa (sha3_of_string (string_of_transaction tr)) in
                        let list_proof_zint = List.map (fun x -> Z.of_string_base 16 x) proof_merkel in
                        let auth_tr = string_of_bool (authenticate hash_tr list_proof_zint merkel_root) in
                        List.iter (fun output ->
                          if String.equal my_account.adress output.adress then
                            begin
                              print_string "\tadress:";
                              print_string output.adress;
                              print_string "  balance_change: +";
                              print_float output.value;print_string " euc";
                              print_newline();
                            end
                          ) tr.outputs;
                          print_string "\tpreuve de la transaction :\n";
                          print_string "\t\tracine de merkel : "; print_string merkel_root;
                          print_newline();
                          print_string "\t\tpreuve de la transaction:\n";
                          List.iter (fun x -> print_string "\t\t\t";print_string x;print_newline()) proof_merkel;
                          print_string "\t\tauthentification: "; print_string auth_tr;
                          print_newline();
                      end
                    
                    ) tr_output
              
    end
  |[]|_ -> print_string "error show tr"; print_newline()