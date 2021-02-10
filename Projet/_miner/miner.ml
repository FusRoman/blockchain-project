open Unix

type miner = {addr : Unix.inet_addr; port : int}


module MinerSet = Set.Make(
  struct 
    type t = miner
    let compare m1 m2 =
      if Unix.string_of_inet_addr m1.addr = Unix.string_of_inet_addr m2.addr && m1.port = m2.port then
        0
      else if m1.port < m2.port then
        -1
      else
        1
  end
)

(* Ensemble des mineurs *)
let set_miner = ref MinerSet.empty

(* L'adresse IP et le port du mineur courant *)
let my_ip = ref "127.0.0.1"
let my_port = ref 8000
let set_my_ip ip = my_ip := ip
let set_my_port port = my_port := port

let exit_miner = ref false

(* La représentation du mineur courant *)
let me = ref {addr = (inet_addr_of_string "127.0.0.1"); port = 0}

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


let string_of_setminer sm =
  let miner_list = MinerSet.elements sm in
  let rec rec_fun l acc = 
    match l with
    |[] -> acc
    |x :: [] -> acc ^ string_of_miner x
    |x :: y :: next ->
      rec_fun (y :: next) (acc ^ string_of_miner x ^ ",") in
  rec_fun miner_list ""

let setminer_of_string string_miner =
  let split_setm = String.split_on_char ',' string_miner in
  List.fold_left (fun set_miner string_miner ->
    MinerSet.add (mineur_of_string string_miner) set_miner) MinerSet.empty split_setm
    