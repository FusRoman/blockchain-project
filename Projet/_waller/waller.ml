open Unix
open Command
open Arg
open Node
open Miscellaneous

let ip_wallet = ref ""
let port_wallet = ref 8000
let set_ip_miner addr = ip_wallet := addr
let set_port_miner port = port_wallet := port

let exit_wallet = ref false

let me = ref { 
    id = 1;
    accounts = [];
    my_internet_adress = (inet_addr_of_string "127.0.0.1", 8000);
    fullnode_info = None;
    connected_account = None
  }

let get_my_connected_info () =
  let ip, port = !me.my_internet_adress in
  (
    ip,
    port
  )

let connect_to_miner distant_miner =
  match String.split_on_char ':' distant_miner with
  |[distant_ip; distant_port] ->
    begin
      let my_ip, my_port = get_my_connected_info () in
      (* Si l'adresse tapé n'est pas celle qui m'a été attribué *)
      if int_of_string distant_port != my_port && distant_ip != string_of_inet_addr my_ip then
        begin
          let s = socket PF_INET SOCK_STREAM 0 in
          setsockopt s SO_REUSEADDR true;

          (* On se connecte au mineur distant et on ouvre un canal d'envoi et de reception *)
          Unix.connect s (ADDR_INET (inet_addr_of_string distant_ip, int_of_string distant_port));
          let in_chan = in_channel_of_descr s in
          let out_chan = out_channel_of_descr s in
          
          (* On envoie les informations du mineur courant vers le mineur connecté *)
          output_value out_chan (New_waller (my_ip, my_port));
          flush out_chan;
          
          
          (* On attend une réponse du mineur connecté *)
          let received_command = input_value in_chan in

          match received_command with
          |Change_info (my_id, _, _) ->
            begin
              let new_me = {
                !me with
                  id = my_id;
                  fullnode_info = (Some (inet_addr_of_string distant_ip, int_of_string distant_port))
              } in
              me := new_me
            end
          |_ -> ();


            Unix.shutdown s Unix.SHUTDOWN_ALL;
            close s
        end
    end
  |_ -> raise (Arg.Bad "mauvais argument, l'adresse doit être de la forme ip:port")


let set_distant_addr addr =

  let tmp_list = String.split_on_char ':' addr in
  match tmp_list with
  |[ip;port] ->
    ip_wallet := ip;
    port_wallet := int_of_string port;
    print_string ("Vous êtes maintenant connecté à " ^ ip ^ ":" ^ port)
  |_ -> raise (Arg.Bad "mauvais argument, l'adresse doit être de la forme ip:port")

let send_msg msg =
  match !me.fullnode_info with
  |None -> ()
  |Some (ip, port) ->
    let s1 = socket PF_INET SOCK_STREAM 0 in
    setsockopt s1 SO_REUSEADDR true;

    connect s1 (ADDR_INET(ip, port));
    let out_chan = out_channel_of_descr s1 in
    output_value out_chan msg;
    flush out_chan;
    shutdown s1 Unix.SHUTDOWN_ALL

let get_account_by_name name_account = 
  List.find (fun acc -> acc.account_name = name_account) !me.accounts

let get_my_fullnode_info () =
  match !me.fullnode_info with
  |None -> ((inet_addr_of_string "127.0.0.1"), 8000)
  |Some (ip, port) -> (ip, port)

let aux_compute_balance output_tr_list =
  match !me.connected_account with
  |None -> -1.0
  |Some s ->
    let my_account = get_account_by_name s in
    List.fold_left (fun acc ((tr:Block.transaction), _, _, _) ->
      List.fold_left (fun acc (output:Block.output_tr) ->
        if String.equal output.adress my_account.adress then
          acc +. output.value
        else
          acc
        ) acc tr.outputs) 0.0 output_tr_list

let compute_account_balance s =
  let my_account = get_account_by_name s in
  let (ip_miner, port_miner) = get_my_fullnode_info () in

  let s = socket PF_INET SOCK_STREAM 0 in
  setsockopt s SO_REUSEADDR true;

  Unix.connect s (ADDR_INET (ip_miner, port_miner));
  let in_chan = in_channel_of_descr s in
  let out_chan = out_channel_of_descr s in

  (* On envoie les informations du mineur courant vers le mineur connecté *)
  let size, n, e = get_public_key my_account.rsa_key in
  output_value out_chan (Request_transaction (my_account.adress, size, n, e));
  flush out_chan;
  
  
  (* On attend une réponse du mineur connecté *)
  let received_command = input_value in_chan in
  
  match received_command with
    |Send_wt (all_my_tr, my_output) ->
      begin
        aux_compute_balance my_output
      end
    |_ -> -1.0

let create_transaction adress value =
  match !me.connected_account with
  |None -> ()
  |Some s ->
    begin
      let my_account = get_account_by_name s in
      let (ip_miner, port_miner) = get_my_fullnode_info () in

      let sock = socket PF_INET SOCK_STREAM 0 in
      setsockopt sock SO_REUSEADDR true;

      Unix.connect sock (ADDR_INET (ip_miner, port_miner));
      let in_chan = in_channel_of_descr sock in
      let out_chan = out_channel_of_descr sock in

      (* On envoie les informations du mineur courant vers le mineur connecté *)
      let size, n, e = get_public_key my_account.rsa_key in
      output_value out_chan (Request_transaction (my_account.adress, size, n, e));
      flush out_chan;
      
      
      (* On attend une réponse du mineur connecté *)
      let received_command = input_value in_chan in
      
      match received_command with
        |Send_wt (all_my_tr, _) ->
          begin
            let account_balance = compute_account_balance s in


            if value > account_balance then
              print_string "Il n'y a pas assez d'argent sur le compte"
            else
              begin
                let rec aux tr_list acc_v acc_tr =
                  match tr_list with
                  |[] -> acc_tr, acc_v
                  |((tr:Block.transaction), _, _, _) :: next ->
                    let balance_tr = List.fold_left (fun acc (out:Block.output_tr) -> if Cryptokit.string_equal my_account.adress out.adress then acc +. out.value else acc) 0.0 tr.outputs in
                    if acc_v +. balance_tr >= value then
                      tr :: acc_tr, acc_v +. balance_tr
                    else
                      aux next (acc_v +. balance_tr) (tr :: acc_tr) in
                
                let needed_tr, value_tr = aux all_my_tr 0.0 [] in   
                let diff_value = value_tr -. value in
                let (diff_output:Block.output_tr) = {
                  value = diff_value;
                  adress = my_account.adress   
                } in
                
                let input_tr = List.map (fun tr ->
                  let hash_tr = sha3_of_string (Block.string_of_transaction tr) in 
                  let (_, id) = List.fold_left (fun (test,id) (out:Block.output_tr) -> if Cryptokit.string_equal my_account.adress out.adress then (true, id) else (test, id + 1)) (false, 0) tr.outputs in
                ({
                  previous_tr_hash = hash_tr;
                  previous_out_index = id;
                  signature = "";
                  public_key = get_public_key my_account.rsa_key
                }:Block.input_tr)) needed_tr in

                let (output_tr:Block.output_tr) = {
                  value;
                  adress 
                } in

                let (new_tr:Block.transaction) = {inputs = input_tr; outputs = [diff_output; output_tr]} in


                let signature = Block.sign_transaction my_account.rsa_key new_tr in
                let new_tr = {
                new_tr with
                inputs = List.map (fun (input:Block.input_tr) -> {input with signature = signature}) new_tr.inputs
                } in
                send_msg (Send_transaction (Lazy, new_tr))
              end
          end
        |_ -> ();
      Unix.shutdown sock Unix.SHUTDOWN_ALL;
      close sock
    end
  
let gen_tr tr_info =
  let tr_split = String.split_on_char '>' tr_info in
  match tr_split with
  |[adress; value] -> create_transaction adress (float_of_string value)
  |[]|_ -> print_string "error gen_tr\n"

let show_my_transaction limit_tr =
  match !me.connected_account with
  |None -> print_string "aucun compte connecté"
  |Some s ->
    begin
      let my_account = get_account_by_name s in
      let (ip_miner, port_miner) = get_my_fullnode_info () in

      let s = socket PF_INET SOCK_STREAM 0 in
      setsockopt s SO_REUSEADDR true;

      Unix.connect s (ADDR_INET (ip_miner, port_miner));
      let in_chan = in_channel_of_descr s in
      let out_chan = out_channel_of_descr s in

      (* On envoie les informations du mineur courant vers le mineur connecté *)
      let size, n, e = get_public_key my_account.rsa_key in
      output_value out_chan (Request_transaction (my_account.adress, size, n, e));
      flush out_chan;
    
    
      (* On attend une réponse du mineur connecté *)
      let received_command = input_value in_chan in

      match received_command with
      |Send_wt (all_tr, my_output) ->
        begin
          List.iteri (fun i ((tr:Block.transaction), merkel_root, proof_merkel, id_bloc_tr) ->
                if i < limit_tr then
                  begin
                    print_string "transaction dans le bloc : ";print_int id_bloc_tr;print_string "  output:\n";
                    let hash_tr = string_to_hexa (sha3_of_string (Block.string_of_transaction tr)) in
                    let list_proof_zint = List.map (fun x -> Z.of_string_base 16 x) proof_merkel in
                    let auth_tr = string_of_bool (authenticate hash_tr list_proof_zint merkel_root) in
                    List.iter (fun (output:Block.output_tr) ->
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
                  ) all_tr;

          List.iteri (fun i ((tr:Block.transaction), merkel_root, proof_merkel, id_bloc_tr) ->
            if i < limit_tr then
              begin
                
                let hash_tr = string_to_hexa (sha3_of_string (Block.string_of_transaction tr)) in
                let list_proof_zint = List.map (fun x -> Z.of_string_base 16 x) proof_merkel in
                let auth_tr = string_of_bool (authenticate hash_tr list_proof_zint merkel_root) in
                List.iter (fun (output:Block.output_tr) ->
                  if String.equal my_account.adress output.adress then
                    begin
                      print_string "transaction dans le bloc : ";print_int id_bloc_tr;print_string "  output:\n";
                      print_string "\tadress:";
                      print_string output.adress;
                      print_string "  balance_change: +";
                      print_float output.value;print_string " euc";
                      print_newline();
                      print_string "\tpreuve de la transaction :\n";
                      print_string "\t\tracine de merkel : "; print_string merkel_root;
                      print_newline();
                      print_string "\t\tpreuve de la transaction:\n";
                      List.iter (fun x -> print_string "\t\t\t";print_string x;print_newline()) proof_merkel;
                      print_string "\t\tauthentification: "; print_string auth_tr;
                      print_newline();
                    end
                  ) tr.outputs;
              end
              ) my_output
        end
      |_ -> ();
      Unix.shutdown s Unix.SHUTDOWN_ALL;
      close s
    end



let debug () =
  ()

let create_account name =
  if not (List.exists (fun account -> account.account_name = name) !me.accounts) then
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
          accounts = new_account :: !me.accounts
        } in
      me := new_me
    end
  else
    begin
      print_string "Le nom de compte existe déjà";
      print_newline()
    end


let connect_account name =
  if (List.exists (fun account -> account.account_name = name) !me.accounts) then
    begin
      let acc = List.find (fun account -> account.account_name = name) !me.accounts in
      let new_me = {
        !me with
          connected_account = (Some acc.account_name) 
      } in
      me := new_me;
      print_string ("Vous êtes maintenant connecté au compte " ^ name)
    end
  else
    begin
      print_string "Le nom de compte n'existe pas";
      print_newline();
    end


let disconnect_account () =
  let new_me = {
    !me with
      connected_account = None
  } in
  me := new_me;
  print_string "Le compte est deconnecté.\n"

let string_of_me () =
  let my_ip, my_port = !me.my_internet_adress in
  string_of_int !me.id ^ "#" ^ string_of_inet_addr my_ip ^ ":" ^ string_of_int my_port

let init_me () =
  (* Permet d'initialisé les informations du mineur courant avec son adresse IP et son numéro de port *)
  let s1 = socket PF_INET SOCK_STREAM 0 in
  setsockopt s1 SO_REUSEADDR true;

  let my_ip = match !ip_wallet with
    |"" -> inet_addr_of_string "127.0.0.1"
    |x -> inet_addr_of_string x in

  let rec test_port acc_port =
    (* Permet de selectionné le premier port libre de la machine si celui indiqué est déjà pris *)
    let current_addr = ADDR_INET(my_ip,acc_port) in
    try
      bind s1 current_addr;
      acc_port
    with
    |Unix.Unix_error (error,ucommand, dir) ->
      test_port (acc_port+1)
      in
  let real_port = test_port !port_wallet in
  me := init_lazynode (my_ip, real_port);
  s1

let show_my_balance () =
  match !me.connected_account with
  |None -> ()
  |Some s ->
    let account_balance = compute_account_balance s in
    print_string ("Solde du compte : " ^ string_of_float account_balance);
    print_newline()

let show_account_info a =
  print_string (string_of_account a);
  print_newline();
  print_string ("balance: " ^ string_of_float (compute_account_balance a.account_name));
  print_newline()

let show_connect_account () =
  match !me.connected_account with
  |None -> ()
  |Some s ->
    show_account_info (get_account_by_name s)

let show_all_account () =
  List.iter (fun a ->
    show_account_info a; print_newline()) !me.accounts

let command_behavior line =
  let listl = String.split_on_char ' ' line in

  let exit = ("-exit", Arg.Set exit_wallet, "  Termine le wallet" ) in
  let show_me = ("-me", Arg.Unit (fun () -> print_string (string_of_me ())), "  Affiche mes informations") in
  let clear = ("-clear", Arg.Unit (fun () -> let _ = Sys.command "clear" in ()), "  Supprime les affichages du terminal") in
  let connect = ("-connect", Arg.String connect_to_miner, " Permet de se connecter à un mineur distant: format -> ip:port") in
  let debug = ("-debug", Arg.Unit (fun () -> debug ()), "  Lance la fonction de débug") in
  let make_tr = ("-send_euc", Arg.String gen_tr, " permet d'envoyer de l'euc à une adresse : format -> adress>value") in
  let show_my_tr = ("-show_tr", Arg.Int show_my_transaction, " Permet de voir toute les transactions du compte") in
  let create_account = ("-new_a", Arg.String create_account, " Créé un nouveau compte et lui donne un nom") in
  let connect_account = ("-co_a", Arg.String connect_account, " Permet de connecter le mineur a un compte. Cela lance le minage sur ce compte.") in
  let disconnect_account = ("-dis_a", Arg.Unit disconnect_account, " Se deconnecte du compte.") in
  let balance = ("-show_balance", Arg.Unit show_my_balance, " Affiche le solde du compte") in
  let show_c_account = ("-show_c", Arg.Unit show_connect_account, " Affiche le compte actuellement connecté") in
  let show_a_account = ("-show_all", Arg.Unit show_all_account, " Affiche tous les comptes") in

  let speclist = [exit; clear; connect; debug; make_tr; show_my_tr; create_account; connect_account; disconnect_account; show_me;
                    balance; show_c_account; show_a_account] in

  parse_command (Array.of_list listl) speclist;
  print_newline ()

let print_commandline () =
  match !me.connected_account with
  |None ->
    print_string ("wallet_"^ string_of_int !me.id ^ ">");
  |Some s ->
    print_string ("wallet_"^ string_of_int !me.id ^ "#" ^ s ^ ">")


let run_wallet () =

  while not !exit_wallet do
    print_commandline();
    let line = read_line() in
    command_behavior line
  done


let () =

  let speclist = [("-miner_addr", Arg.Tuple [Arg.String set_ip_miner; Arg.Int set_port_miner], " Spécifie l'adresse du waller")]
  in let usage_msg = "Création d'un waller de la blockchain. Options disponible:"
  in Arg.parse speclist print_endline usage_msg;

  let s1 = init_me() in
  run_wallet();
  Unix.shutdown s1 Unix.SHUTDOWN_ALL;
  close s1;

  