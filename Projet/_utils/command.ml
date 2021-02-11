open Miner


(* Commande de communication entre les wallers *)
type waller_command =
|Transaction of string
|Transac_proof of string

and

(* Commande de communication entre les mineurs *)
    serv_command =
|New_miner of miner
|Recv_minerset of MinerSet.t
|Connected_miner of miner
|Waller_message of waller_command
|Broadcast of serv_command

exception ServCommandError

let rec string_of_waller_command wc =
  match wc with
  |Transaction s -> "transaction#" ^ s
  |Transac_proof sp -> "transaction_proof#" ^ sp

let rec waller_command_of_string string_wc =
  let tmp = String.split_on_char '#' string_wc in
  match tmp with
  |[x;y] ->
    begin
      match x with
      |"transaction" -> Transaction y
      |"transac_proof" -> Transac_proof y
      |_ -> raise ServCommandError
    end
  |_ -> raise ServCommandError

let rec string_of_serv_command sc =
  match sc with
  |New_miner m -> "new_miner-" ^ string_of_miner m
  |Recv_minerset ml -> "recv_minerlist-" ^ string_of_setminer ml
  |Connected_miner m -> "connected_miner-" ^ string_of_miner m
  |Waller_message m -> "waller_message-" ^ (string_of_waller_command m)
  |Broadcast m -> "broadcast-" ^ string_of_serv_command m


let rec serv_command_of_string string_sc =
  let tmp = String.split_on_char '-' string_sc in
  match tmp with
  [x;y] ->
    begin
      match x with
      |"new_miner" -> New_miner (mineur_of_string y)
      |"recv_minerlist" -> Recv_minerset (setminer_of_string y)
      |"connected_miner" -> Connected_miner (mineur_of_string y)
      |"waller_message" -> Waller_message (waller_command_of_string y)
      |_ -> raise ServCommandError
    end
  |[x;y;z] ->
    begin
      match x with
      |"broadcast" -> Broadcast (serv_command_of_string (y ^ "-" ^ z))
      |_ -> raise ServCommandError
    end
  |_ -> raise ServCommandError

  let failwithf fmt = Printf.ksprintf (fun s -> print_string s) fmt
  let s_ str = str
  let f_ (str: ('a, 'b, 'c, 'd) format4) = str
  
  let parse_command argv args =
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