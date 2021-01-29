open Unix

type miner = {addr : Unix.inet_addr; port : int}

let string_of_miner m =
  "{" ^ string_of_inet_addr m.addr ^ ":" ^ string_of_int m.port ^ "}"

let strip_both_chars str =
  match String.length str with
    | 0 | 1 | 2 -> ""
    | len -> String.sub str 1 (len - 2)

exception ErrorMiner
let mineur_of_string string_m =
  let tmp = strip_both_chars string_m in
  let addr_and_port = String.split_on_char ':' tmp in
  match addr_and_port with
  |[x;y] -> 
    let addr = inet_addr_of_string x in
    let port = int_of_string y in
    {addr;port}
  |_ -> raise ErrorMiner

let string_of_listminer lm =
  let rec rec_fun l acc = 
    match l with
    |[] -> acc
    |x :: [] -> acc ^ string_of_miner x
    |x :: y :: next ->
      rec_fun (y :: next) (acc ^ string_of_miner x ^ ",") in
  rec_fun lm ""

let listminer_of_string string_miner =
  let split_listm = String.split_on_char ',' string_miner in
  List.map (fun str_miner ->
    mineur_of_string str_miner) split_listm