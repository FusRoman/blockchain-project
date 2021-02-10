open Digest
open Marshal
open Unix
open Thread
open Mutex
open Z


(* Version des block pour le projet *)
type block = {m : string; id : int; mutable nonce : int; previous_hash: string; timestamp: float}


let make_genesis m =
  {m; nonce = 0; id = 0; previous_hash = ""; timestamp = time()}

let make_block m id previous_hash =
  {m = m^(string_of_int id); nonce = 0; id; previous_hash; timestamp = time()}

let hash_block b =
  Digest.string (Marshal.to_string b [])





(* Version des block pour les exercices *)

(*
type block = {mutable m : string; mutable nonce : string; id : int}

let difficulty = 4


let make_b i m = {m = m; nonce = ""; id = i}


let verif_nonce hash =
  let test = String.make difficulty '0' in
  let head_hash = String.sub hash 0 difficulty in
  test = head_hash

let proof_of_work block =
  let init_length = String.length block.m in
  let rec tmp_proof i =
    block.m <- block.m ^ (string_of_int i);
    let h_block = Digest.string (Marshal.to_string block []) in

    let hexa_h = Digest.to_hex h_block in
    if verif_nonce hexa_h then
      hexa_h
    else
      begin block.m <- String.sub block.m 0 init_length;
      tmp_proof (i+1) end in
    tmp_proof 0


let parallel_proof_of_work m =
  let block_find = ref false in
  let res_block = ref (make_b 0 m) in
  let protect_block_find = Mutex.create () in


  let rec tmp_proof i =
    if !block_find then
      begin
      print_string ("Thread " ^ string_of_int (Thread.id (Thread.self ())) ^ " a perdu");
      print_newline();  
      exit()
      end
    else
      let current_block = make_b 0 (m ^ string_of_int i) in
      let h_block = Digest.string (Marshal.to_string current_block []) in

      let hexa_h = Digest.to_hex h_block in

      Mutex.lock protect_block_find;
      let value_block_find = !block_find in
      Mutex.unlock protect_block_find;

      if not value_block_find then
        begin
          if verif_nonce hexa_h then
            begin
              (* Zone d'exclusion mutuelle :
                  Un seul thread peut trouvé un block et peut donc modifié les propriétés du block
                  de départ et annoncé au autre que le block a été trouvé *)
              Mutex.lock protect_block_find;
              current_block.nonce <- hexa_h;
              res_block := current_block;
              block_find := true;
              print_string ("Thread " ^ string_of_int (Thread.id (Thread.self ())) ^ " a gagné");
              print_newline();
              print_string ("block trouvé : " ^ hexa_h);
              print_newline();
              Mutex.unlock protect_block_find
            end
          else
            tmp_proof (i+2)
        end 
      else
        exit() in
  
  let block_pair = Thread.create tmp_proof 0 in
  let block_impair = Thread.create tmp_proof 1 in
  Thread.join block_pair;
  Thread.join block_impair;
  !res_block
  
  

let blocks n =
  List.init n (fun i -> make_b i "blabla")

let split_list l =
  (* Conserve l'ordre des blocks *)
  let length_of_l = List.length l in
  let rec tmp_split l1 l2 origin_l i =
    match origin_l with
    |[] -> (l1, l2)
    |x :: next -> 
      if i < (length_of_l / 2) then
        tmp_split (x :: l1) l2 next (i+1)
      else
        tmp_split l1 (x :: l2) next (i+1) in
  tmp_split [] [] l 0

let mining_block_list lb = 
  List.iter (fun block ->
    let nonce = proof_of_work block in
    block.nonce <- nonce
    ) lb


let parallel_mining_block_list lb =
  let list_left, list_right = split_list lb in
  let mine_ll = Thread.create mining_block_list list_left in
  let mine_l2 = Thread.create mining_block_list list_right in
  join mine_ll;
  join mine_l2;
  List.concat [list_right; list_left]


let exo () =
  (* minage d'une liste de block en parallèle *)
  
    (*let list_blocks = blocks 20 in
    
  
    let block_list_mined = parallel_mining_block_list list_blocks in
    
    List.iter (fun block ->
      print_int block.id;
      print_newline();
      print_string block.m;
      print_newline();
      print_string block.nonce;
      print_newline()
      ) block_list_mined;
      print_newline();
      print_newline();*)
  
      (* minage d'un block en parallèle *)
  
      let b = parallel_proof_of_work "blablatoto" in
      print_int b.id;
      print_newline();
      print_string b.m;
      print_newline();
      print_string b.nonce;
      print_newline();
      print_newline()

*)