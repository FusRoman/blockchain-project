open Unix
open Command
open Arg
open Cryptokit
open Node

let ip_miner = ref ""
let port_miner = ref 0
let set_ip_miner addr = ip_miner := addr
let set_port_miner port = port_miner := port

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
          output_value out_chan (New_waller (my_ip, my_port));
          flush out_chan;
          
          
          (* On attend une réponse du mineur connecté *)
          let received_command = input_value in_chan in

          match received_command with
          |Change_info (my_id, new_dns, new_blockchain) ->
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
    ip_miner := ip;
    port_miner := int_of_string port;
    print_string ("Vous êtes maintenant connecté à " ^ ip ^ ":" ^ port)
  |_ -> raise (Arg.Bad "mauvais argument, l'adresse doit être de la forme ip:port")

let send_msg msg =
  let s1 = socket PF_INET SOCK_STREAM 0 in
  setsockopt s1 SO_REUSEADDR true;

  connect s1 (ADDR_INET(inet_addr_of_string !ip_miner, !port_miner));
  let out_chan = out_channel_of_descr s1 in
  output_value out_chan msg;
  flush out_chan;
  shutdown s1 Unix.SHUTDOWN_ALL

let debug () =
  ()

let command_behavior line =
  let listl = String.split_on_char ' ' line in

  let exit = ("-exit", Arg.Set exit_wallet, "  Termine le wallet" ) in
  let clear = ("-clear", Arg.Unit (fun () -> let _ = Sys.command "clear" in ()), "  Supprime les affichages du terminal") in
  let connect = ("-connect", Arg.String set_distant_addr, " Initialise l'adresse d'un mineur distant, l'argument doit être de la forme ip:port") in
  let send = ("-send", Arg.String send_msg, " Permet d'envoyer un message vers l'adresse spécifié par la commande -connect") in
  let debug = ("-debug", Arg.Unit (fun () -> debug ()), "  Lance la fonction de débug") in

  let speclist = [exit; clear; connect; send; debug] in

  parse_command (Array.of_list listl) speclist;
  print_newline ()

let run_wallet () =

  while not !exit_wallet do
    let line = read_line() in
    command_behavior line
  done


let () =

  let speclist = [("-miner_addr", Arg.Tuple [Arg.String set_ip_miner; Arg.Int set_port_miner], " Spécifie l'adresse du waller")]
  in let usage_msg = "Création d'un waller de la blockchain. Options disponible:"
  in Arg.parse speclist print_endline usage_msg;

  run_wallet()

  