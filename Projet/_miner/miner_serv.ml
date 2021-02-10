open Unix
open Thread
open Condition
open Miner
open Command
open Mutex
open Block

let connect_to_miner distant_miner =

  match String.split_on_char ':' distant_miner with
  |[ip; port] ->
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
        set_miner := MinerSet.union !set_miner lm;
        set_miner := MinerSet.add {addr = inet_addr_of_string ip; port = int_of_string port} !set_miner
      |New_miner m ->
        print_string "nm";print_newline();
      |Connected_miner m ->
        print_string "cm";print_newline();
      |Waller_message _ |Broadcast _ -> ();

      
      shutdown s Unix.SHUTDOWN_ALL
    end
  |_ -> raise (Arg.Bad "mauvais argument, l'adresse doit être de la forme ip:port")



let rec broadcast_miner message =
  (* Problème si un mineur s'est déco et qu'il est toujours dans la liste -> rattraper l'exception ECONNREFUSED *)
  MinerSet.iter (fun miner -> 
    (* On créé la socket puis on la connecte vers le mineur de la liste *)
    let s = socket PF_INET SOCK_STREAM 0 in
    setsockopt s SO_REUSEADDR true;
    Unix.connect s (ADDR_INET(miner.addr, miner.port));

    (* On prépare le message à envoyer. Il s'agit du nouveau mineur à envoyé vers tous les autres *)
    let out_chan = out_channel_of_descr s in

    (* On envoie le message *)
    output_value out_chan message;
    flush out_chan;

    (*On ferme la socket et on passe au mineur suivant *)
    Unix.shutdown s Unix.SHUTDOWN_ALL
    ) !set_miner



(* Fonction permettant de terminé proprement un thread tout en fermant correctement la socket passé en argument*)
let properly_close sc =
  Unix.shutdown sc Unix.SHUTDOWN_ALL;
  Thread.exit()

(*
    Fonction gérant les messages entrant pour le mineur
*)
let receive_msg sc =
  match select [sc] [] [] 5.0 with
  | [], [], [] -> properly_close sc
  | sc_list, [], [] ->
    begin
      if List.mem sc sc_list then
        begin
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
              broadcast_miner (Broadcast (New_miner m));

              (* On ajoute le nouveau mineur à notre liste *)
              set_miner := MinerSet.add m !set_miner
            |Waller_message m ->
              (* Reception d'un message provenant d'un waller *)
              let waller_msg = string_of_waller_command m in
              print_string waller_msg;
              print_newline();
              broadcast_miner (Broadcast (Waller_message m))
            |Broadcast m ->
              (* Reception d'un message de broadcast *)
              begin
                match m with
                |New_miner mi ->
                  (* Un nouveau mineur a été broadcasté *)

                  (* Ajout du nouveau mineur à la liste *)
                  set_miner := MinerSet.add mi !set_miner
                |Waller_message m ->
                  (* Reception d'un message de waller broadcasté *)
                  let waller_msg = string_of_waller_command m in
                  print_string waller_msg;
                  print_newline();
                |_ -> ()
            end;

            Unix.shutdown sc Unix.SHUTDOWN_ALL

          with End_of_file ->
            properly_close sc
        end
      else
        properly_close sc
    end
    |_, _, _ -> properly_close sc

(*
  Fonction permettant l'attente d'une connexion au serveur. Chaque nouvelle connexion crée un thread traitant la connexion.
*)
  let rec serv_process sock =
    listen sock 5;
    match select [sock] [] [] (2.0) with
    |[], [], [] ->
      if !exit_miner then Thread.exit() else serv_process sock
    |sc_list, [], [] ->
      begin
        if List.mem sock sc_list then
        begin
            let sc, _ = accept sock in
            let _ = Thread.create receive_msg sc in
            serv_process sock
        end
      end
    |_, _, _ -> print_string "error"

  let failwithf fmt = Printf.ksprintf (fun s -> print_string s) fmt
  let s_ str = str
  let f_ (str: ('a, 'b, 'c, 'd) format4) = str

  let parse argv args =
    (* Simulate command line for Arg *)
    let current =
      ref 0
    in

    try
      Arg.parse_argv
        ~current:current
        (Array.concat [[|"none"|]; argv])
        (Arg.align args)
        (failwithf (f_ "Don't know what to do with arguments: '%s'"))
        (s_ "configure options:")
    with
      | Arg.Help txt ->
        print_endline txt
      | Arg.Bad txt ->
        prerr_endline txt
      |Unix.Unix_error (error, ucommand, dir) -> print_string "Error : ";print_string (Unix.error_message error)

let parse_line line =
  let listl = String.split_on_char ' ' line in

  let connect = ("-connect", Arg.String connect_to_miner, "   connexion à un mineur distant") in
  let exit = ("-exit", Arg.Set exit_miner, "  Termine le mineur" ) in
  let show_miner = ("-show_miner", Arg.Unit (fun () -> print_string (string_of_setminer ())), "  Affiche la liste des mineurs connue") in
  let show_me = ("-me", Arg.Unit (fun () -> print_string (string_of_miner !me)), "  Affiche mes informations") in
  let clear = ("-clear", Arg.Unit (fun () -> let _ = Sys.command "clear" in ()), "  Supprime les affichages du terminal") in

  let speclist = [connect; exit; show_miner; show_me; clear] in

  parse (Array.of_list listl) speclist;
  print_newline ()

let rec run_miner s1 =

  let _ = Thread.create serv_process s1 in

  while true do
    print_string ("miner@" ^ (string_of_miner !me) ^ ">");
    let line = read_line() in
    parse_line line
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


(* version parallèle du mineur*)
let () =
  (* On parse les arguments en ligne de commande *)
  let exec_speclist = [("-my_addr", Arg.Tuple [Arg.String set_my_ip; Arg.Int set_my_port], " Spécifie l'adresse du mineur")] in
  let exec_usage = "Création d'un mineur de la blockchain. Options disponible:" in
  Arg.parse exec_speclist print_endline exec_usage;

  (* création de la prise de ce mineur *)
  let s1 = init_me () in

  (* Lancement du mineur *)
  run_miner s1