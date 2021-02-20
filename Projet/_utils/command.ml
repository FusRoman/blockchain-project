open Block
open Node



(* Commande de communication entre les mineurs *)
type 'a serv_command =
  (*New_miner permet d'envoyer les infos nécessaires vers le mineur distant. Ces infos sont
        - l'adresse du mineur (ip, port)
        - l'ensemble des adresses de compte associés
        - l'ensemble des mineurs qu'il connait *)
|New_miner of Unix.inet_addr * int * string list * DNS.t
|Change_id_and_dns of int * DNS.t
|Broadcast of 'a serv_command * 'a

exception ServCommandError

(*
let rec string_of_serv_command sc =
  match sc with
  |New_miner m -> "new_miner-" ^ string_of_miner m
  |Recv_minerset ml -> "recv_minerlist-" ^ string_of_setminer ml
  |Connected_miner m -> "connected_miner-" ^ string_of_miner m
  |Broadcast m -> "broadcast-" ^ string_of_serv_command m
  |Transaction t -> "transaction-" ^ t
  |Transac_proof tp -> "transac_proof-" ^ tp


let rec serv_command_of_string string_sc =
  let tmp = String.split_on_char '-' string_sc in
  match tmp with
  [x;y] ->
    begin
      match x with
      |"new_miner" -> New_miner (mineur_of_string y)
      |"recv_minerlist" -> Recv_minerset (setminer_of_string y)
      |"connected_miner" -> Connected_miner (mineur_of_string y)
      |"transaction" -> Transaction y
      |"transac_proof" -> Transac_proof y
      |_ -> raise ServCommandError
    end
  |[x;y;z] ->
    begin
      match x with
      |"broadcast" -> Broadcast (serv_command_of_string (y ^ "-" ^ z))
      |_ -> raise ServCommandError
    end
  |_ -> raise ServCommandError
*)



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