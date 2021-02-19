open Unix
open Thread
open Mutex
open Cryptokit
open Miscellaneous

(*
==========================================================================================================================================================================================================
  Le type de la cryptmonnaie de notre blockchain
*)
type euc = float


(*
==========================================================================================================================================================================================================
  Représentation des transactions de la blockchain
*)

(*
  L'entrée d'une transaction est une référence à la sortie d'une d'une transaction précédente.
  Plusieurs entrée peuvent être référencé dans une transaction et toute les valeurs d'entrée
  d'une nouvelle transaction (c'est à dire la valeur total des sortie de la transactions référence)
  sont ajouté. Le total doit être complétement utilisé par la transaction.

  C'est à dire que la somme total des sortie référencé par chaque entré doit être
  égale à la somme total des sortie de la nouvelle transaction.

  Le champ previous_tr_hash permet de faire référence à la transaction précédente.
  Le champ previous_out_index permet de faire référence à la sortie spécifique de
  la transaction référencé par previous_hash.

  Chaque entrée contient également un champ signature et un champ public_key.

  La clé public est utilisé pour permettre l'utilisation du input par le compte
  associé à la clé public. Concrétement, si le hash de la clé public correspond
  au champ adress de la sortie référencé alors l'entré est autorisé à être utilisé.
  Le champ signature permet de vérifié l'identité de l'émetteur du input.
  La signature est calculé avec un hash de la version simplifié de la transaction.

  Il existe un input spécial nommé "Generation". Il ne fait référence a aucun output particulier et permet de crée de nouvelle devise de la monnaie virtuelle.
  Chaque mineur intégrant un bloc ajoute au début de la liste d'input de la transaction un input Génération.
  L'output correspondant est crée et le mineur y place son adresse. Le montant contenue dans le output est fixé à 50 au lancement de la blockchain.
  Le montant contenue dans la generation est diminué de moitié a chaque pallier atteint. Cela permet de limité le nombre total de monnaie qu'il sera possible de mettre en
  circulation.
  Le champ previous_tr_hash est appelé le coinbase et sert de base à la transaction "Generation". Le previous_tr_hash contient un champ "extraNonce" expliqué plus tard. 
*)
type input_tr = {
  previous_tr_hash: string;
  previous_out_index: int;
  signature: string;
  public_key: int * string * string
}


(*
  Une sortie d'une transaction.
  La valeur correspond à la quantité d'euc contenue dans la sortie.
  Le champ adress correspond aux destinataire de cette sortie.
*)
type output_tr = {
  value: euc;
  adress: string
}


(*
  Une transaction correspond à un ensemble d'entré et de sortie.
  Les valeurs échangés dans la transaction correspondent aux total des valeurs
  contenue dans la liste d'entré.
  Comme énoncé plus haut, la somme des valeurs des inputs doit être
  la même que la somme des outputs.

  Si un utilisateur souhaite échangé moins d'euc que le total d'input, il doit crée
  un output spécial contenant la différence avec pour destinataire sa propre adresse.
  Tout euc prevenant d'un input et non contenue dans un output est considéré comme
  frais de transaction que le mineur peut récupéré.
*)
type transaction = {
inputs: input_tr list;
outputs: output_tr list
}


let string_of_input_tr itr =
  itr.previous_tr_hash ^
  string_of_int itr.previous_out_index

let string_of_output_tr otr =
  string_of_float otr.value ^
  otr.adress


let string_of_transaction tr =
  (List.fold_left (fun acc input ->
    acc ^ string_of_input_tr input) "" tr.inputs) ^
  (List.fold_left (fun acc output ->
    acc ^ string_of_output_tr output) "" tr.outputs)


let sign_transaction (my_private_key: RSA.key) tr =
  let hash_tr = sha3_of_string (string_of_transaction tr) in
  RSA.sign my_private_key hash_tr

let verif_transaction transaction_signature pk_component tr  =
  let pk = make_public_key pk_component in
  let verif_tr = RSA.unwrap_signature pk transaction_signature in
  let hash_tr = sha3_of_string (string_of_transaction tr) in
  Cryptokit.string_equal (remove_null_characters verif_tr) hash_tr


(*
==========================================================================================================================================================================================================
    Représentation des blocs de la blockchain
*)

(*
  Représentation du header d'un bloc.
  Le header contient le hash du bloc précédent. Le hash d'un bloc est le hash de son header.

  Le header contient la racine de l'arbre de merkel des transactions.

  Le header contient le timestamp du block. Un timestamp est considéré valide si il est 
  plus grand que le moyenne des timestamp des 11 blocs précédent. Le timestamp doit
  être également plus petit que le temps ajusté du réseau plus deux heure.
  Le temps ajusté du réseau est la moyenne des timestamp renvoyé par tous les noeuds 
  connecté. Lorsqu'un noeud se connecte à un autre noeud, il récupère son timestamp
  actuelle et ne sauvegarde que la différence avec son propre timestamp.
  Ainsi il n'est pas necéssaire que les noeuds se communique leurs timestamp constamment.

  La cible est la difficulté de minage du bloc. La difficulté est ajusté de sorte a avoir un bloc miné toute les 10 min. Le target est modifié tous les 2016 blocs.
  Chaque full node compare le temps qu'il a mis pour miné la bloc à la position x avec le temps mis pour miné le bloc à la position x - 2016. Le target est réajusté selon un pourcentage
  de la différence des deux temps de minage. Un changement de target ne peut être fait d'un facteur supérieur à 4 pour éviter de trop gros changement de difficulté.

  Le nonce est incrémenté à chaque test du proof_of_work pour modifié la valeur du hash. Comme le nonce est représenté par un entier ayant une représentation binaire limité,
  chaque fois que le nonce overflow, un champ de la transaction "Generation" du bloc est incrémenté, ce champ est appelé "extraNonce" et fait partie 
*)
type block_header = {
  previous_hash: string; 
  hash_merkelroot: string; 
  timestamp: float; 
  target: Z.t; 
  nonce: int
  }

(*
  Un bloc contient son header et la liste des transactions contenue dans le bloc.

  Lorsqu'un bloc est miné, les transactions continue d'être ajouté à la liste. Il faut par conséquent recalculé le champ hash_merkelroot.
*)
type block = {
  block_h: block_header;
  transactions: transaction list
  }

(*
 Le bloc genesis est le premier bloc de la blockchain.
*)
let block_genesis =
  {
    block_h = {
      previous_hash = "";
      hash_merkelroot = "";
      timestamp = 0.0;
      target = Z.of_int 0;
      nonce = -1
    };
    transactions = []
  }




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