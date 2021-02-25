open Unix
open Thread
open Condition
open Miner
open Command
open Mutex
open Block
open Cryptokit
open Miscellaneous
open Node

(* Cette fonction permet de créé un comportement de timeout sur une socket
    -le premier paramètre est un n-uplet dont:
        -timeout [float] -> permet de spécifier le temps en seconde du timeout
        -working_function -> une fonction a appelé si le file descriptor sc reçoit des données en lecture avant le timeout
        -args_work -> les arguments de la fonction working_function
        -timeout_function -> une fonction a appelé si le timeout se déclenche, c'est a dire que le file descriptor sc n'a reçu aucune donnée en lecture 
                                  durant la période du timeout
        -args_timeout -> les arguments de la fonction timeout_function
        -error_function -> une fonction a appelé en cas d'erreur quelconque 
        -args_error -> les arguments de la fonction error_function
    sc -> le file descriptor sur lequel sera mis en place le timeout *)
let read_socket_timeout (timeout, working_function, args_work, timeout_function, args_timeout, error_function, args_error) sc =
  match select [sc] [] [] timeout with
  | [], [], [] ->  timeout_function args_timeout
  | sc_list, [], [] ->
      if List.mem sc sc_list then
        working_function args_work
      else
        error_function args_error
  |_, _, _ -> error_function args_error

exception Command_error of string


(*
        Cette fonction permet d'initier la connexion initial entre deux mineurs
*)
let connect_to_miner distant_miner =
  match String.split_on_char ':' distant_miner with
  |[distant_ip; distant_port] ->
    begin
      let my_ip, my_port, account_adress, my_dns = get_my_connected_info () in
      let test_exist_dns = DNS.filter (fun dns_t -> let ip, port = dns_t.internet_adress in
                                                    port = int_of_string distant_port && String.equal distant_ip (string_of_inet_addr ip)) !me.dns in
      (* Si l'adresse tapé n'est pas celle qui m'a été attribué *)
      if int_of_string distant_port != my_port && distant_ip != string_of_inet_addr my_ip && (DNS.is_empty test_exist_dns) then
        begin
          let s = socket PF_INET SOCK_STREAM 0 in
          setsockopt s SO_REUSEADDR true;

          (* On se connecte au mineur distant et on ouvre un canal d'envoi et de reception *)
          Unix.connect s (ADDR_INET (inet_addr_of_string distant_ip, int_of_string distant_port));
          let in_chan = in_channel_of_descr s in
          let out_chan = out_channel_of_descr s in
           
          (* On envoie les informations du mineur courant vers le mineur connecté *)
          output_value out_chan (New_miner (my_ip, my_port, my_dns));
          flush out_chan;
          
          if DNS.is_empty !me.dns then
            begin
              (* On attend une réponse du mineur connecté *)
              let received_command = input_value in_chan in

              match received_command with
              |Change_info (my_id, new_dns, new_blockchain) ->
                begin
                  let new_me = {
                    !me with
                    lazy_part = {
                      !me.lazy_part with
                      id = my_id
                    };
                    dns = new_dns;
                    blockchain = new_blockchain
                  } in
                  update_me new_me
                end
              |_ -> ();

            end;

            Unix.shutdown s Unix.SHUTDOWN_ALL;
            close s
        end
    end
  |_ -> raise (Arg.Bad "mauvais argument, l'adresse doit être de la forme ip:port")



(* Fonction permettant de terminé proprement un thread tout en fermant correctement la socket passé en argument*)
let properly_close sc =
  Unix.shutdown sc Unix.SHUTDOWN_ALL;
  close sc;
  Thread.exit()

(*
    Fonction gérant les messages entrant pour le mineur
*)
let receive_msg sc =
  try
    let in_chan = in_channel_of_descr sc in
    let out_chan = out_channel_of_descr sc in

  
    (* On receptionne un message sur le canal *)
    let received_message = input_value in_chan in
    begin
      (* On traite la commande correspondant au message*)
      match received_message with
      |New_miner (distant_ip, distant_port, distant_dns) ->
        begin
          (* Un nouveau mineur se connecte *)
          let my_dns_t = {id = !me.lazy_part.id; internet_adress = !me.lazy_part.my_internet_adress} in

          (* On test si son DNS est vide ou non *)
          if not (DNS.is_empty distant_dns) then
            begin

              (* Son DNS n'est pas vide. Cela signifie qu'il y a d'autre mineurs qui sont connecté à celui-ci.
                  Il faut réattribué les ids de chacun des mineurs que conneit le mineur qui s'est connecté à nous de façon à ce que les les ids de tous le monde soit unique *)
              let new_dns, all_new_id = merge_and_return_new_dns distant_dns !me.dns in

              (* On donne un id unique au mineurs qui s'est connecté à nous. *)
              let free_id = get_free_id new_dns !me.lazy_part.id in
              (* On crée son dns. *)
              let new_dns = DNS.add {
                id = free_id;
                internet_adress = (distant_ip, distant_port)
              } new_dns in

              (* On envoie le nouveau dns a tous les mineurs sans exceptions *)
              DNS.iter (fun dns_t ->
                let real_new_dns = DNS.add my_dns_t new_dns in
                send_msg_to_miner dns_t.id new_dns (Change_info (dns_t.id, real_new_dns, !me.blockchain))) new_dns;

              (* On met a jour notre propre dns. *)
              let new_me = {
                !me with
                dns = new_dns
              } in
              update_me new_me
            end
          else
            begin
              (* Le dns du nouveau mineur est vide *)
              let free_id = get_free_id !me.dns !me.lazy_part.id in

              (* On lui envoie son id unique et on ajoute notre propre dns a son dns *)
              output_value out_chan (Change_info (free_id, DNS.add my_dns_t !me.dns, !me.blockchain));
              flush out_chan;

              (* On transmet l'info a tous les mineurs que l'on connait qu'un nouveau mineur est arrivé. *)
              broadcast_miner !me.dns (fun m -> m) (Broadcast (New_miner (distant_ip, distant_port, distant_dns), free_id));

              (* On ajoute le nouveau mineur a notre dns. *)
              let (new_dns_t: Node.dns_translation) = {
                id = free_id;
                internet_adress = (distant_ip, distant_port)
              } in
              let new_me = {
                !me with
                dns = DNS.add new_dns_t !me.dns
              } in
              update_me new_me
            end
        end
        |New_waller (ip, port) -> 
        let free_id = get_free_id !me.dns !me.lazy_part.id in
        output_value out_chan (Change_info (free_id, !me.dns, !me.blockchain));
        flush out_chan;
        (* On transmet l'info a tous les mineurs que l'on connait qu'un nouveau mineur est arrivé. *)
        broadcast_miner !me.dns (fun m -> m) (Broadcast (New_waller (distant_ip, distant_port), free_id));

        (* On ajoute le nouveau mineur a notre dns. *)
        let (new_dns_t: Node.dns_translation) = {
          id = free_id;
          internet_adress = (distant_ip, distant_port)
        } in
        let new_me = {
          !me with
          dns = DNS.add new_dns_t !me.dns
        } in
        update_me new_me

      |Broadcast (m, id) ->
        begin
          match m with
          |New_miner (distant_ip, distant_port, new_dns) ->
            let (new_dns_t: Node.dns_translation) = {
            id;
            internet_adress = (distant_ip, distant_port)
          } in
          let new_me = {
                !me with
                dns = DNS.add new_dns_t !me.dns
              } in
          update_me new_me
          |New_waller (distant_ip, distant_port) ->
           let (new_dns_t: Node.dns_translation) = {
            id;
            internet_adress = (distant_ip, distant_port)
          } in
          let new_me = {
                !me with
                dns = DNS.add new_dns_t !me.dns
              } in
          update_me new_me
          |_ -> ()
        end
        |Request_transaction (_, _, _, _) ->
        ()
        |Send_wt _ -> ()
      |Change_info (new_id, new_dns, new_blockchain) ->
        (* Un changement d'id arrive. Le mineur met a jour son id et son dns *)
        let real_new_dns = DNS.filter (fun dns_t -> dns_t.id != new_id) new_dns in
        let new_me = {
          !me with
          lazy_part = {
            !me.lazy_part with
            id = new_id
          };
          dns = real_new_dns;
          blockchain = if (List.length new_blockchain) > (List.length !me.blockchain) then new_blockchain else !me.blockchain
        } in
        update_me new_me
      |New_block (sender_id, pos_block, block) ->
        begin
          lock mutex_new_bloc;
            
          let blockchain_length = List.length !me.blockchain in
          if pos_block = blockchain_length + 1 then
            begin
              if verif_block block.block_h then
                begin
                  notify_new_block := true;
                  let new_me = {
                    !me with
                    blockchain = block :: !me.blockchain
                  } in
                  update_me new_me;
                  if not (verif_blockchain !me.blockchain) then
                    print_string "ERROOOOOOOOOOOOR !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!";
                  unlock mutex_new_bloc
                end
              else
                begin
                  unlock mutex_new_bloc
                end
            end
          else if pos_block >= blockchain_length then
            begin
              send_msg_to_miner sender_id !me.dns (Request_blockchain !me.lazy_part.id );
              unlock mutex_new_bloc
            end
          else
            begin
              unlock mutex_new_bloc
            end
        end
      |Request_blockchain sender_id ->
        begin
          send_msg_to_miner sender_id !me.dns (Send_blockchain !me.blockchain)
        end
      |Send_blockchain new_blockchain ->
        begin
          lock mutex_new_bloc;
          notify_new_block := true;
          let new_me = {
            !me with
            blockchain = new_blockchain
          } in
          update_me new_me;

          unlock mutex_new_bloc
        end
      |Send_transaction tr ->
        begin
          lock mutex_new_transaction;
          new_transaction := tr :: !new_transaction;
          unlock mutex_new_transaction
        end
    end;

    properly_close sc

  with 
  |End_of_file ->
    properly_close sc;
    ()
  |Unix_error (what_error, msg1, msg2) ->
    begin
      properly_close sc;
      print_string (error_message what_error);
    match what_error with
    |Unix.EPIPE ->
      begin
        print_string "error Broken pipe\n"
      end
    |_ ->
      begin
        print_string "receive msg other error\n"
      end
    end

(*
  Fonction permettant l'attente d'une connexion au serveur. Chaque nouvelle connexion crée un thread traitant la connexion.
*)
let rec serv_process sock =
  try
    listen sock 40;
    while not !exit_miner do
      let sc, _ = accept sock in
      let _ = Thread.create receive_msg sc in
      flush_all ();
      ()
    done
  with
  |Unix_error (what_error, msg1, msg2) ->
    begin
      print_string (error_message what_error);
      print_newline();
    match what_error with
    |Unix.EPIPE ->
      begin
        print_string "error Broken pipe\n";
        serv_process sock
      end
    |_ ->
      begin
        print_string "serv process other error\n";
        serv_process sock
      end
    end


(* Fonction de débug permettant de tester des trucs dans l'environnement du mineur *)
let debug () =
  print_newline();

  

  print_newline();

  let addr = "1" in
  let value = 104.04 in
  create_transaction_for_miners addr value;

  print_newline();
  (*
  print_string "affichage de la blockchain";
  print_newline();

  List.iteri (fun i b -> print_string "bloc numero :";print_int i;print_newline(); print_bloc b) !me.blockchain;*)

  print_newline()
  
  

let command_behavior line =
  let listl = String.split_on_char ' ' line in

  let connect = ("-connect", Arg.String connect_to_miner, "   connexion à un mineur distant : format -> ip:port") in
  let exit = ("-exit", Arg.Set exit_miner, "  Termine le mineur" ) in
  let show_miner = ("-show_node", Arg.Unit (fun () -> print_string (Node.string_of_dns !me.dns)), "  Affiche la liste des noeuds connue") in
  let show_me = ("-me", Arg.Unit (fun () -> print_string (string_of_me ())), "  Affiche mes informations") in
  let clear = ("-clear", Arg.Unit (fun () -> let _ = Sys.command "clear" in ()), "  Supprime les affichages du terminal") in
  let debug = ("-debug", Arg.Unit (fun () -> debug ()), "  Lance la fonction de débug") in
  let create_account = ("-new_a", Arg.String create_account, " Créé un nouveau compte et lui donne un nom") in
  let connect_account = ("-co_a", Arg.String connect_account, " Permet de connecter le mineur a un compte. Cela lance le minage sur ce compte.") in
  let disconnect_account = ("-dis_a", Arg.Unit disconnect_account, " Se deconnecte du compte.") in
  let stat_blockchain = ("-stat_chain", Arg.Unit stat_chain, " Affiche les statistiques de la blockchain") in
  let balance = ("-show_balance", Arg.Unit show_my_balance, " Affiche le solde du compte") in
  let show_c_account = ("-show_c", Arg.Unit show_connect_account, " Affiche le compte actuellement connecté") in
  let show_a_account = ("-show_all", Arg.Unit show_all_account, " Affiche tous les comptes") in
  let show_exist_adress = ("-show_ad", Arg.Unit show_all_adress, " Affiche les adresses de compte existant") in
  let make_tr = ("-send_euc", Arg.String gen_tr, " permet d'envoyer de l'euc à une adresse : format -> adress>value") in
  let show_my_tr = ("-show_tr", Arg.String show_my_transaction, " Permet de voir toute les transactions du compte") in

  let speclist = [connect; exit; show_miner; show_me; clear; debug; create_account; connect_account; disconnect_account; stat_blockchain; balance;
                    show_c_account; show_a_account; show_exist_adress; make_tr; show_my_tr] in

  parse_command (Array.of_list listl) speclist;
  print_newline ()

let print_commandline () =
  match !me.lazy_part.connected_account with
  |None ->
    print_string ("miner_"^ string_of_int !me.lazy_part.id ^ ">");
  |Some s ->
    print_string ("miner_"^ string_of_int !me.lazy_part.id ^ "#" ^ s ^ ">")

let rec run_miner s1 =
  let _ = Thread.create serv_process s1 in
  let _ = Thread.create mine_block Z.zero in

  try
    while not !exit_miner do
      print_commandline();
      let line = read_line() in
      command_behavior line
    done
  with
  |Unix_error (what_error, msg1, msg2) ->
    begin
      print_string (error_message what_error);
    match what_error with
    |Unix.EPIPE ->
      begin
        print_string "error Broken pipe\n";
        run_miner s1
      end
    |_ ->
      begin
        print_string "run miner other error\n";
        run_miner s1
      end
    end


(* Permet d'initialiser le mineur courant *)
let init_me () =
  (* Permet d'initialisé les informations du mineur courant avec son adresse IP et son numéro de port *)
  let s1 = socket PF_INET SOCK_STREAM 0 in
  setsockopt s1 SO_REUSEADDR true;

  let my_ip = match !my_ip with
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
  let real_port = test_port !my_port in
  update_me (init_fullnode (my_ip, real_port));
  s1

let () =
  (* On parse les arguments en ligne de commande *)
  let exec_speclist = [("-my_addr", Arg.Tuple [Arg.String set_my_ip; Arg.Int set_my_port], " Spécifie l'adresse du mineur")] in
  let exec_usage = "Création d'un mineur de la blockchain. Options disponible:" in
  Arg.parse exec_speclist print_endline exec_usage;

  (* création de la prise de ce mineur *)
  let s1 = init_me () in
  
  (* Lancement du mineur *)
  try
    run_miner s1
  with
  |Unix_error (what_error, msg1, msg2) ->
    begin
      print_string (error_message what_error);
    match what_error with
    |Unix.EPIPE ->
      begin
        print_string "error Broken pipe\n";
        ()
      end
    |_ ->
      begin
        print_string "main other error\n";
        ()
      end
    end