open Unix
open Command
open Arg
open Cryptokit


let ip_miner = ref ""
let port_miner = ref 0
let set_ip_miner addr = ip_miner := addr
let set_port_miner port = port_miner := port

let exit_wallet = ref false

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
  output_value out_chan (Transaction msg);
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

  let speclist = [("-miner_addr", Arg.Tuple [Arg.String set_ip_miner; Arg.Int set_port_miner], " Spécifie l'adresse du mineur")]
  in let usage_msg = "Création d'un waller de la blockchain. Options disponible:"
  in Arg.parse speclist print_endline usage_msg;

  run_wallet()

  