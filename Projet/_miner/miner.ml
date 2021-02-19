open Unix
open Node


(* L'adresse IP et le port du mineur courant *)
let my_ip = ref "127.0.0.1"
let my_port = ref 8000
let set_my_ip ip = my_ip := ip
let set_my_port port = my_port := port

let exit_miner = ref false

(* La représentation du mineur courant *)
let me = ref { 
  lazy_part = {
    id = 1;
    accounts = [];
    fullnode_info = None
  }; 
  dns = DNS.empty
  } 
    