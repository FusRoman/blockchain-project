open Block
open Node



(* Commande de communication entre les mineurs *)
type 'a serv_command =
  (*New_miner permet d'envoyer les infos nécessaires vers le mineur distant. Ces infos sont
        - l'adresse du mineur (ip, port)
        - l'ensemble des mineurs qu'il connait *)
|New_miner of Unix.inet_addr * int * DNS.t
|New_waller of Unix.inet_addr * int
|Change_info of int * DNS.t * block list
|Broadcast of 'a serv_command * 'a
|New_block of int * int * block
|Request_blockchain of int
|Send_blockchain of block list
|Send_transaction of type_node * transaction
(*
  Request_transaction permet de demander les transactions d'un compte d'un waller à un miner
    -adresse du compte
    -clé public sous la forme (size, n, e)
*)
|Request_transaction of string * int * string * string
(*
  Permet a un miner d'envoyer les transactions qu'a demander un waller
*)
|Send_wt of (Block.transaction * string * string list * int) list * (Block.transaction * string * string list * int) list

exception ServCommandError

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