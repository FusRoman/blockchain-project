open Miner

type serv_command =
|New_miner of miner
|Recv_minerlist of miner list
|Connected_miner of miner
|Waller_message of string
|Broadcast of serv_command

let rec string_of_serv_command sc =
  match sc with
  |New_miner m -> "new_miner-" ^ string_of_miner m
  |Recv_minerlist ml -> "recv_minerlist-" ^ string_of_listminer ml
  |Connected_miner m -> "connected_miner-" ^ string_of_miner m
  |Waller_message m -> "waller_message-" ^ m
  |Broadcast m -> "broadcast-" ^ string_of_serv_command m

exception ServCommandError

let rec serv_command_of_string string_sc =
  let tmp = String.split_on_char '-' string_sc in
  match tmp with
  [x;y] ->
    begin
      match x with
      |"new_miner" -> New_miner (mineur_of_string y)
      |"recv_minerlist" -> Recv_minerlist (listminer_of_string y)
      |"connected_miner" -> Connected_miner (mineur_of_string y)
      |"waller_message" -> Waller_message y
      |_ -> raise ServCommandError
    end
  |[x;y;z] ->
    begin
      match x with
      |"broadcast" -> Broadcast (serv_command_of_string (y ^ "-" ^ z))
      |_ -> raise ServCommandError
    end
  |_ -> raise ServCommandError

(* Fonction à appeler dès que l'on souhaite envoyé une chaine de caractères par une socket.
    Ajoute un \n à la fin du message pour que la fonction input_line puissent se débloquer*)
let prepare_send_message m =
  m ^ "\n"