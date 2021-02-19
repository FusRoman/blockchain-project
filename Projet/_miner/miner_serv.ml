open Unix
open Thread
open Condition
open Miner
open Command
open Mutex
open Block
open Cryptokit
open Miscellaneous

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


(*
        Cette fonction permet d'initier la connexion initial entre deux mineurs
*)
let connect_to_miner distant_miner =
  match String.split_on_char ':' distant_miner with
  |[ip; port] ->
    begin
      (* Si l'adresse tapé n'est pas celle qui m'a été attribué *)
      if int_of_string port != !me.port && ip != string_of_inet_addr !me.addr then
        begin
          let s = socket PF_INET SOCK_STREAM 0 in
          setsockopt s SO_REUSEADDR true;

          (* On se connecte au mineur distant et on ouvre un canal d'envoi et de reception *)
          Unix.connect s (ADDR_INET(inet_addr_of_string ip, int_of_string port));
          let in_chan = in_channel_of_descr s in
          let out_chan = out_channel_of_descr s in
          
          (* On envoie les informations du mineur courant vers le mineur connecté *)
          output_value out_chan (Connected_miner !me);
          flush out_chan;
          

          (* On attend une réponse du mineur connecté *)
          let received_command = input_value in_chan in

          match received_command with
          |Recv_minerset lm ->
            (* On initialise notre liste avec la liste venant du mineur auquel nous sommes connecté. On y ajoute également le mineur auquel nous sommes connecté.*)
            let filter = (MinerSet.filter (fun miner -> miner.addr != !me.addr && miner.port != !me.port) lm) in
            set_miner := MinerSet.union !set_miner filter;
            set_miner := MinerSet.add {addr = inet_addr_of_string ip; port = int_of_string port} !set_miner;
          |New_miner m ->
            print_string "nm";print_newline();
          |Connected_miner m ->
            print_string "cm";print_newline();
          |Transaction _ |Broadcast _ |Transac_proof _ -> ();

          
          shutdown s Unix.SHUTDOWN_ALL
        end
    end
  |_ -> raise (Arg.Bad "mauvais argument, l'adresse doit être de la forme ip:port")

  
type broadcast_feedback =
|Ok
|Error


let rec broadcast_miner (message, connecting_addr) predicates =
  (* Problème si un mineur s'est déco et qu'il est toujours dans la liste -> rattraper l'exception ECONNREFUSED *)
  MinerSet.iter (fun miner ->
      begin
        (* On créé la socket puis on la connecte vers le mineur de la liste *)
        let s = socket PF_INET SOCK_STREAM 0 in
        setsockopt s SO_REUSEADDR true;
        Unix.connect s (ADDR_INET(miner.addr, miner.port));

        (* On prépare le message à envoyer. *)
        let out_chan = out_channel_of_descr s in

        (* On envoie le message *)
        output_value out_chan message;
        flush out_chan;

        (*On ferme la socket et on passe au mineur suivant *)
        Unix.shutdown s Unix.SHUTDOWN_ALL
      end
    ) !set_miner



(* Fonction permettant de terminé proprement un thread tout en fermant correctement la socket passé en argument*)
let properly_close sc =
  Unix.shutdown sc Unix.SHUTDOWN_ALL;
  Thread.exit()

(*
    Fonction gérant les messages entrant pour le mineur
*)
let receive_msg (sc, connecting_addr) =
  
  let in_chan = in_channel_of_descr sc in
  let out_chan = out_channel_of_descr sc in

  try
    (* On receptionne un message sur le canal *)
    let received_message = input_value in_chan in

    (* On traite la commande correspondant au message*)
    match received_message with
    |New_miner m ->
      ()
    |Recv_minerset lm ->
      ()
    |Connected_miner m ->
      (* Connexion d'un nouveau mineur *)
    
      (* Envoie de notre liste de mineur au nouveau mineur *)
      output_value out_chan (Recv_minerset !set_miner);
      (* On oublie pas de vider le canal de sortie *)
      flush out_chan;

      (* On broadcast le nouveau mineur vers tous les autres que l'on connait *)
      broadcast_miner ((Broadcast (New_miner m)), connecting_addr) ();

      (* On ajoute le nouveau mineur à notre liste *)
      set_miner := MinerSet.add m !set_miner
    |Transaction m ->
      (* Reception d'un message provenant d'un waller *)
      let hash_received_msg = zint_of_hash (string_of_serv_command (Transaction m)) in
      set_msg_received := IntSet.add hash_received_msg !set_msg_received;
      
      print_string m;
      print_newline();
      broadcast_miner ((Broadcast (Transaction m)), connecting_addr) ()
    |Transac_proof tp -> ()
    |Broadcast m ->
      begin
        (* Reception d'un message de broadcast *)
        (* On crée le hash du message *)
        let hash_received_msg = zint_of_hash (string_of_serv_command m) in
        
        (* Si le message n'est pas dans l'ensemble des messages reçu *)
        if not (already_received hash_received_msg) then
          begin
            (* On ajoute le message à l'ensemble des messages déjà reçu *)
            set_msg_received := IntSet.add hash_received_msg !set_msg_received;
            match m with
            |New_miner mi ->
              (* Un nouveau mineur a été broadcasté *)

              (* Ajout du nouveau mineur à la liste si le nouveau mineur n'est pas déjà moi *)
              if mi.addr != !me.addr && mi.port != !me.port then
                set_miner := MinerSet.add mi !set_miner
            |Transaction m ->
              (* Reception d'un message de waller broadcasté *)
              print_string m;
              print_newline();
              broadcast_miner ((Broadcast (Transaction m)), connecting_addr) ()
            |_ -> ()
          end
      end;
    Unix.shutdown sc Unix.SHUTDOWN_ALL
  with End_of_file ->
    properly_close sc

(*
  Fonction permettant l'attente d'une connexion au serveur. Chaque nouvelle connexion crée un thread traitant la connexion.
*)
let rec serv_process sock =
  listen sock 5;

  let server_handler sock =
    let sc, connecting_addr = accept sock in
    (* On met en place un timeout de 10 seconde pour la reception d'un message sur la socket; au dela de 10 seconde, le thread est terminé *)
    let received_msg_handling = read_socket_timeout (10.0, receive_msg, (sc, connecting_addr), properly_close, sc, (fun () -> print_string "erreur sur la réception d'un message"), ()) in
    (* On lance le thread chargé de traité le message entrant *)
    let _ = Thread.create received_msg_handling sc in
    serv_process sock in
  
  let server_timeout sock =
    set_msg_received := IntSet.empty;
    serv_process sock in

  (* On met en place un timeout de 4 seconde. Toute les 2 secondes, l'ensemble des messages reçu est remis à zéro *)
  read_socket_timeout (4.0, server_handler, sock, server_timeout, sock, (fun () -> print_string "erreur sur le traitement du serveur"), ()) sock


(* Fonction de débug permettant de tester des trucs dans l'environnement du mineur *)
let debug () =
  let hw_rng = Random.hardware_rng() in
  
  let fake_tr = {inputs = []; outputs = []} in

  let user_0_key = RSA.new_key ~rng:hw_rng 2048 in
  let user_1_key = RSA.new_key ~rng:hw_rng 2048 in
  let pk_0 = get_public_key user_0_key in
  let pk_1 = get_public_key user_1_key in
  let sign = sign_transaction user_0_key fake_tr in
  let r = verif_transaction sign pk_0 fake_tr in
  print_string (string_of_bool r) 
  

let command_behavior line =
  let listl = String.split_on_char ' ' line in

  let connect = ("-connect", Arg.String connect_to_miner, "   connexion à un mineur distant") in
  let exit = ("-exit", Arg.Set exit_miner, "  Termine le mineur" ) in
  let show_miner = ("-show_miner", Arg.Unit (fun () -> print_string (string_of_setminer !set_miner)), "  Affiche la liste des mineurs connue") in
  let show_me = ("-me", Arg.Unit (fun () -> print_string (string_of_miner !me)), "  Affiche mes informations") in
  let clear = ("-clear", Arg.Unit (fun () -> let _ = Sys.command "clear" in ()), "  Supprime les affichages du terminal") in
  let debug = ("-debug", Arg.Unit (fun () -> debug ()), "  Lance la fonction de débug") in

  let speclist = [connect; exit; show_miner; show_me; clear; debug] in

  parse_command (Array.of_list listl) speclist;
  print_newline ()

let run_miner s1 =

  let _ = Thread.create serv_process s1 in

  while not !exit_miner do
    print_string ("miner@" ^ (string_of_miner !me) ^ ">");
    let line = read_line() in
    command_behavior line
  done


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
  me := {addr = my_ip; port = real_port};
  s1

let () =
  (* On parse les arguments en ligne de commande *)
  let exec_speclist = [("-my_addr", Arg.Tuple [Arg.String set_my_ip; Arg.Int set_my_port], " Spécifie l'adresse du mineur")] in
  let exec_usage = "Création d'un mineur de la blockchain. Options disponible:" in
  Arg.parse exec_speclist print_endline exec_usage;

  (* création de la prise de ce mineur *)
  let s1 = init_me () in

  (* Lancement du mineur *)
  run_miner s1