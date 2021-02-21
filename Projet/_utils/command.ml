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