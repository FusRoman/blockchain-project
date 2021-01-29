open Unix
open Command

let ip_miner = ref ""
let port_miner = ref 0
let set_ip_miner addr = ip_miner := addr
let set_port_miner port = port_miner := port


let () =

  let speclist = [("-miner_addr", Arg.Tuple [Arg.String set_ip_miner; Arg.Int set_port_miner], " Spécifie l'adresse du mineur")]
  in let usage_msg = "Création d'un waller de la blockchain. Options disponible:"
  in Arg.parse speclist print_endline usage_msg;

  let s1 = socket PF_INET SOCK_STREAM 0 in
  setsockopt s1 SO_REUSEADDR true;

  connect s1 (ADDR_INET(inet_addr_of_string !ip_miner, !port_miner));
  let out_chan = out_channel_of_descr s1 in

  (*let in_chan, out_chan = open_connection addr*)
  

  let m =  read_line() in
  if m = "quit" then (close s1; exit 0);

  let send_m = prepare_send_message (string_of_serv_command (Waller_message m)) in
  output_string out_chan (send_m);
  flush out_chan