open Unix
open Node
open Command
open Cryptokit
open Miscellaneous


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
    my_internet_adress = (inet_addr_of_string "127.0.0.1", 8000);
    fullnode_info = None
  }; 
  dns = DNS.empty
  }

let get_all_accounts_adress () =
  List.fold_left (fun adress account ->
    account.adress :: adress) [] !me.lazy_part.accounts 

let get_my_connected_info () =
  let ip, port = !me.lazy_part.my_internet_adress in
  (
    ip,
    port,
    get_all_accounts_adress(),
    !me.dns
  )

let update_me my_id new_dns =
  me := {
    lazy_part = {
      !me.lazy_part with
      id = my_id
    };
    dns = DNS.union !me.dns new_dns
  }

let get_free_id dns id =
  if DNS.is_empty dns then
    id + 1
  else
    let rec aux acc =
      let test1 = (DNS.exists (fun dns_t -> dns_t.id = acc) dns) in
      let test2 = id = acc in
      if test1 || test2 then
        aux (acc + 1)
      else
        acc in
    aux 1

let string_of_me () =
  let my_ip, my_port = !me.lazy_part.my_internet_adress in

  
  string_of_int !me.lazy_part.id ^ "#" ^ string_of_inet_addr my_ip ^ ":" ^ string_of_int my_port


let merge_and_return_new_dns dns_1 dns_2 =
  let last_dns_list = DNS.elements dns_1 in
  let rec aux l1 set_l2 new_id_list =
    match l1 with
    |[] -> set_l2, new_id_list
    |dns_t :: next ->
      let free_id = get_free_id set_l2 !me.lazy_part.id in
      aux next (DNS.add {dns_t with id = free_id} set_l2) ((dns_t.internet_adress, free_id) :: new_id_list) in
  aux last_dns_list dns_2 []


let create_account name =
  let account_key = RSA.new_key 128 in
  print_string "ok";
  let new_account = {
    account_name = name;
    euc_balance = 0.0;
    rsa_key = account_key;
    adress = hash_of_public_key (get_public_key account_key);
    transaction = []
  } in
  me := {
    !me with
    lazy_part = {
      !me.lazy_part with
      accounts = new_account :: !me.lazy_part.accounts
    }
  }
