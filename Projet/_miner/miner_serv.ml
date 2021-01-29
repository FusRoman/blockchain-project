open Unix
open Thread
open Miner
open Command



let my_ip = ref "127.0.0.1"
let my_port = ref 8000
let set_my_ip ip = my_ip := ip
let set_my_port port = my_port := port
let init_me ip port =
  let my_ip = match ip with
    |"" -> inet_addr_of_string "127.0.0.1"
    |x -> inet_addr_of_string x in
  {addr = my_ip; port}, ADDR_INET(my_ip,port)

let rec add_list_without_double l1 l2 =
  match l1 with
    |[] -> l2
    |x::next ->
      if not (List.exists (fun current_miner -> x.addr == current_miner.addr && x.port == current_miner.port ) l2) then
        add_list_without_double next (x :: l2)
      else
        add_list_without_double next l2


let rec broadcast_miner lm message =
  (* Problème si un mineur s'est déco et qu'il est toujours dans la liste -> rattraper l'exception ECONNREFUSED *)
  match lm with
  |[] -> ()
  | miner :: next ->
    (* On créé la socket puis on la connecte vers le mineur de la liste *)
    let s = socket PF_INET SOCK_STREAM 0 in
    setsockopt s SO_REUSEADDR true;
    connect s (ADDR_INET(miner.addr, miner.port));

    (* On prépare le message à envoyer. Il s'agit du nouveau mineur à envoyé vers tous les autres *)
    let out_chan = out_channel_of_descr s in

    (* On envoie le message *)
    output_string out_chan message;
    flush out_chan;

    (*On ferme la socket et on passe au mineur suivant *)
    close s;
    broadcast_miner next message

(* Adresse Ip et port du mineur auquel se connecté si le mineur n'est pas le premier *)
let ip_miner = ref ""
let port_miner = ref 0
let set_ip_miner addr = ip_miner := addr
let set_port_miner port = port_miner := port

let (listminer : miner list ref) = ref []

let notify_new_miner me =
  if !ip_miner != "" && !port_miner != 0 then
    let s = socket PF_INET SOCK_STREAM 0 in
    setsockopt s SO_REUSEADDR true;
    connect s (ADDR_INET(inet_addr_of_string !ip_miner, !port_miner));
    let in_chan = in_channel_of_descr s in
    let out_chan = out_channel_of_descr s in
    
    let m =  string_of_serv_command (Connected_miner me) in
    output_string out_chan (prepare_send_message m);
    flush out_chan;

    let r = input_line in_chan in
    Format.printf "%s@." r;
    try
      match serv_command_of_string r with
      |Recv_minerlist lm -> 
        print_string "Reception de la liste de mineur";print_newline();
        (* On initialise notre liste avec la liste venant du mineur auquel nous sommes connecté. On y ajoute également le mineur auquel nous sommes connecté.*)
        listminer := {addr = inet_addr_of_string !ip_miner;port = !port_miner} :: lm;
        print_string ("my list : " ^ (string_of_listminer !listminer));print_newline()
      |New_miner m ->
        print_string "nm";print_newline();
      |Connected_miner m ->
        print_string "cm";print_newline();
      |Waller_message _ |Broadcast _ -> ();
      with ErrorMiner ->
        begin
          print_string "Je suis le deuxième mineur";print_newline();
          listminer := [{addr = inet_addr_of_string !ip_miner;port = !port_miner}];
          print_string ("my list : " ^ (string_of_listminer !listminer));print_newline()
        end;
    close s

let in_channel (sock,addr) = 
  bind sock addr;
    
  listen sock 5;
  while true do

    print_string "en attente de connexion ...";print_newline();
    let sc, _ = accept sock in
    
    print_string "Reception d'une connexion.";
    print_newline();
    
    let in_chan = in_channel_of_descr sc in
    let out_chan = out_channel_of_descr sc in

    try
      let m = input_line in_chan in
      match serv_command_of_string m with
      |New_miner m ->
        ()
      |Recv_minerlist lm ->
        ();
      |Connected_miner m -> 
        print_string "Un nouveau mineur se connecte.";
        print_newline();
        print_string ("Il s'agit de " ^ (string_of_miner m));
        print_newline();
      
        (* Envoie de notre liste de mineur au nouveau mineur *)
        let r = string_of_serv_command (Recv_minerlist !listminer) in
        output_string out_chan (prepare_send_message r);
        (* On oublie pas de vider le canal de sortie *)
        flush out_chan;

        (* On broadcast le nouveau mineur vers tous les autres que l'on connait *)
        let message_mineur = prepare_send_message (string_of_serv_command (Broadcast (New_miner m))) in
        broadcast_miner !listminer message_mineur;

        (* On ajoute le nouveau mineur à notre liste *)
        listminer := m :: !listminer;
        print_string ("new list : "^string_of_listminer !listminer); print_newline()
      |Waller_message m ->
        print_string m;
        print_newline();
        broadcast_miner !listminer (string_of_serv_command (Broadcast (Waller_message m)))
      |Broadcast m ->
        begin
          match m with
          |New_miner mi ->
            print_string "Reception d'un nouveau mineur";
            print_newline();

            (* Ajout du nouveau mineur à la liste *)
            listminer := mi :: !listminer;
            print_string ("Ma liste est maintenant : " ^ string_of_listminer !listminer);
            print_newline();
          |Waller_message m ->
            print_string m;
            print_newline();
          |_ -> ()
        end
    with End_of_file -> ();
    close sc;
  done;
  close sock


(* version parallèle du mineur*)
let () =
  let speclist = [("-distant_ip", Arg.String set_ip_miner, " Spécifie l'adresse auquel le mineur doit se connecter");
  ("-distant_port", Arg.Int set_port_miner, " Spécifie le numéro de port auquel le mineur doit se connecter");
  ("-my_addr", Arg.Tuple [Arg.String set_my_ip; Arg.Int set_my_port], " Spécifie l'adresse du mineur")
  ]
  in let usage_msg = "Création d'un mineur de la blockchain. Options disponible:"
  in Arg.parse speclist print_endline usage_msg;

  (* création de la prise de ce mineur *)
  let me, my_addr = init_me !my_ip !my_port in
  print_string ("me : " ^ string_of_miner me);print_newline();
  let s1 = socket PF_INET SOCK_STREAM 0 in
  setsockopt s1 SO_REUSEADDR true;

  (* Notifie au mineur connecté qu'un nouveau mineur arrive et permet de récupéré sa liste de mineur *)
  notify_new_miner me;

  let serveur_thread = create in_channel (s1, my_addr) in

  while true do
    let t = read_line() in
    match t with
    |"quit" -> kill serveur_thread;exit()
    |x-> print_string x;print_newline()
  done;








(* version sequentielle du mineur
let () =
    let speclist = [("-distant_ip", Arg.String set_ip_miner, " Spécifie l'adresse auquel le mineur doit se connecter");
    ("-distant_port", Arg.Int set_port_miner, " Spécifie le numéro de port auquel le mineur doit se connecter");
    ("-my_addr", Arg.Tuple [Arg.String set_my_ip; Arg.Int set_my_port], " Spécifie l'adresse du mineur")
    ]
    in let usage_msg = "Création d'un mineur de la blockchain. Options disponible:"
    in Arg.parse speclist print_endline usage_msg;

    (* création de la prise de ce mineur *)
    let me, my_addr = init_me !my_ip !my_port in
    print_string ("me : " ^ string_of_miner me);print_newline();
    let s1 = socket PF_INET SOCK_STREAM 0 in
    setsockopt s1 SO_REUSEADDR true;

    bind s1 my_addr;
    
    listen s1 5;
    
    (* Notifie au mineur connecté qu'un nouveau mineur arrive et permet de récupéré sa liste de mineur *)
    begin
    if !ip_miner != "" && !port_miner != 0 then
      let s = socket PF_INET SOCK_STREAM 0 in
      setsockopt s SO_REUSEADDR true;
      connect s (ADDR_INET(inet_addr_of_string !ip_miner, !port_miner));
      let in_chan = in_channel_of_descr s in
      let out_chan = out_channel_of_descr s in
      
      let m =  string_of_serv_command (Connected_miner me) in
      output_string out_chan (prepare_send_message m);
      flush out_chan;

      let r = input_line in_chan in
      Format.printf "%s@." r;
      try
        match serv_command_of_string r with
        |Recv_minerlist lm -> 
          print_string "Reception de la liste de mineur";print_newline();
          (* On initialise notre liste avec la liste venant du mineur auquel nous sommes connecté. On y ajoute également le mineur auquel nous sommes connecté.*)
          listminer := {addr = inet_addr_of_string !ip_miner;port = !port_miner} :: lm;
          print_string ("my list : " ^ (string_of_listminer !listminer));print_newline()
        |New_miner m ->
          print_string "nm";print_newline();
        |Connected_miner m ->
          print_string "cm";print_newline();
        |Waller_message _ |Broadcast _ -> ();
        with ErrorMiner ->
          begin
            print_string "Je suis le deuxième mineur";print_newline();
            listminer := [{addr = inet_addr_of_string !ip_miner;port = !port_miner}];
            print_string ("my list : " ^ (string_of_listminer !listminer));print_newline()
          end;
      close s
    end;

    (* Mise en place permanente du mineur *)
      while true do

        print_string "en attente de connexion ...";print_newline();
        let sc, _ = accept s1 in
        
        print_string "Reception d'une connexion.";
        print_newline();
        
        let in_chan = in_channel_of_descr sc in
        let out_chan = out_channel_of_descr sc in
    
        try
          let m = input_line in_chan in
          print_string m; print_newline();
          match serv_command_of_string m with
          |New_miner m ->
            ()
          |Recv_minerlist lm ->
            ();
          |Connected_miner m -> 
            print_string "Un nouveau mineur se connecte.";
            print_newline();
            print_string ("Il s'agit de " ^ (string_of_miner m));
            print_newline();
          
            (* Envoie de notre liste de mineur au nouveau mineur *)
            let r = string_of_serv_command (Recv_minerlist !listminer) in
            output_string out_chan (prepare_send_message r);
            (* On oublie pas de vider le canal de sortie *)
            flush out_chan;

            (* On broadcast le nouveau mineur vers tous les autres que l'on connait *)
            let message_mineur = prepare_send_message (string_of_serv_command (Broadcast (New_miner m))) in
            broadcast_miner !listminer message_mineur;

            (* On ajoute le nouveau mineur à notre liste *)
            listminer := m :: !listminer;
            print_string ("new list : "^string_of_listminer !listminer); print_newline()
          |Waller_message m ->
            print_string m;
            print_newline();
            broadcast_miner !listminer (string_of_serv_command (Broadcast (Waller_message m)))
          |Broadcast m ->
            begin
              match m with
              |New_miner mi ->
                print_string "Reception d'un nouveau mineur";
                print_newline();

                (* Ajout du nouveau mineur à la liste *)
                listminer := mi :: !listminer;
                print_string ("Ma liste est maintenant : " ^ string_of_listminer !listminer);
                print_newline();
              |Waller_message m ->
                print_string m;
                print_newline();
              |_ -> ()
            end
        with End_of_file -> ();
        close sc;
      done;
      close s1
*)