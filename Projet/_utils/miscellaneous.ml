open Cryptokit


(*
  Fonction permettant de supprimer les caractères présent dans la chaine cs de la chaine s.
*)
let stripchars s cs =
  let len = String.length s in
  let res = Bytes.create len in
  let rec aux i j =
    if i >= len
    then Bytes.to_string (Bytes.sub res 0 j)
    else if String.contains cs s.[i] then
      aux (succ i) (j)
    else begin
      Bytes.set res j s.[i];
      aux (succ i) (succ j)
    end
  in
  aux 0 0


(*
  Appel spécifique de la fonction précédente permettant de supprimer les caractères NULL (code ASCII = 0) d'une chaine de caractères.
*)
let remove_null_characters s =
  stripchars s (String.make 1 (Char.chr 0))

(*
======================================================================================================================================================================
  Ensemble de fonction permettant la gestion des opérations cryptographique du projet.
*)


(*
  Convertie une string au format hexadecimale
*)
let string_to_hexa s =
  let to_hex = Hexa.encode() in
  to_hex#put_string s;
  to_hex#finish;
  to_hex#get_string


(*
  Produit le hash d'une chaine de caractère en utilisant l'algorithme sha3 du module HASH de la bibliothèque Cryptokit.
  Le hash produit à une longueur de 256 bits.
*)
let sha3_of_string m =
  let sha256 = Hash.sha3 256 in
  sha256#add_string m;
  sha256#result

let sha3_of_zint z =
  let z_string = Z.to_string z in
  sha3_of_string z_string

let zint_of_hash h =
  let hex_s = string_to_hexa h in
  Z.of_string_base 16 hex_s

let hash_zint z =
  zint_of_hash (sha3_of_zint z)
  
let get_public_key (key: RSA.key) =
  key.size, key.n, key.e

let make_public_key (size, n, e) =
  ({
    size = size;
    n = n;
    e = e;
    d = "";
    p = "";
    q = "";
    dp = "";
    dq = "";
    qinv = ""
  }: RSA.key)

let hash_of_public_key (size, n, e) =
  let pk_string = string_of_int size ^ n ^ e in
  sha3_of_string pk_string

let combine_hash h1 h2 =
  let z1 = zint_of_hash h1 in
  let z2 = zint_of_hash h2 in
  let z_combine = Z.add z1 z2 in
  Z.format "%x" z_combine



(*
======================================================================================================================================================================
  Arbre de Merkel
  Utiliser par les blocs pour stockers les hash des transactions et faciliter la vérification
*)

type merkel_tree =
|Leaf
|Node of Z.t * merkel_tree * merkel_tree * int

let empty = Leaf

let make_leaf x = Node (hash_zint x, Leaf, Leaf, 0)

let make_node t1 t2 =
  match t1, t2 with
  |Node (x1, _, _, lvl1), Node (x2, _, _, lvl2) ->
    let lvl = max lvl1 lvl2 + 1 in
    Node (hash_zint (Z.add x1 x2), t1, t2, lvl)
  |_ -> assert false

let rec fusion lt =
  match lt with
  |[] | [_] -> lt
  | t1 :: t2 :: s -> make_node t1 t2 :: fusion s

let rec merkel_of_list l =
  match l with
  |[] -> Leaf
  |[t] -> t
  |_ -> merkel_of_list (fusion l)


let make l =
  let z_l = List.map (Z.of_string_base 16) l in
  merkel_of_list (List.map make_leaf z_l)

let hash_root t =
  match t with
  |Leaf -> raise Not_found
  |Node (x, _, _, _) -> Z.format "%x" x


let rec proof t i =
  match t with
  |Leaf |Node (_, Leaf, Leaf, _) -> []
  |Node (_, g, d, lvl) ->
    let b = 1 lsl (lvl - 1) in
    if i < b then
      (hash_root d) :: (proof g i)
    else
      (hash_root g) :: (proof d (i - b))

let authenticate tr pr root =
  let tr = Z.of_string_base 16 tr in
  let root = Z.of_string_base 16 root in
  let x =
    List.fold_right (fun h x -> hash_zint (Z.add h x)) pr (hash_zint tr) in
  x = root