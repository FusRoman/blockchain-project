open Unix
open Block
open Miscellaneous

(* 
  Un compte est une collection de transaction envoyé ou reçu à l'adresse qui appartient au compte.

  Chaque compte contient une liste de transaction. Chaque transaction contient l' id de la transaction, la date à laquelle elle a été confirmé, 
  une adresse et le montant de cryptomonnaie transféré par la transaction.
  

  Chaque compte est associé à une pair de clé privé/public pour garantir la sécurité du compte via le protocole RSA.
  Un compte a également un nom donné par l'utilisateur pour la commodité.

  Le compte ne contient pas d'argent bien qu'une variable euc_balance (Earth Unified Currency) existe. Celle ci est juste un indicateur de nombre de euc 
  associé à cette adresse après le parcours des transactions confirmé dans la blockchain. Pour calculer l'euc_balance, il faut parcourir les transactions de la blockchain et
  récupéré tout les outputs de chaque transactions. On vérifie ensuite que ces outputs ne sont pas référencé par des transactions plus récente. Si c'est le cas, le champs value 
  de cette output est ajouté à la balance.
*)
type account = {
  account_name : string;
  rsa_key: Cryptokit.RSA.key;
  adress: string
  }

let string_of_account a =
  "name : " ^ a.account_name ^
  "\nadress : " ^ a.adress


(*
  Deux types de noeuds existe dans le système distribué.
  Un lazy node est un noeud uniquement capable de faire des transactions. Il est identifié par un id unique à tous les noeuds (qu'il soit lazy ou full) 
  du système distribué.
  Le lazy node contient une liste de compte. Chaque compte est capable d'effectué des transaction.

  Un lazy node contient également les informations du full node (mineur) auquel il est connecté. Ce mineur sera chargé de traiter les transactions de ce lazy node.
  Les informations que stocke le lazy node à propos du full node prend la forme d'un triplet. Le premier entier est l'identifiant du full node.
  Le couple suivant est la paire (IP, port) permettant de se connecter à ce full node.
  Ces informations sont en options car les full nodes sont également des lazy nodes. Ils n'ont donc pas besoin d'être connecté à un autre mineurs
  pour traité leurs transactions puisqu'il peuvent le faire eux-même. 
*)
type lazy_node = {
  id: int;
  accounts: account list;
  my_internet_adress: Unix.inet_addr * int;
  mutable fullnode_info: (int * (Unix.inet_addr * int)) option;
  connected_account: string option
  }


(*
  Pour permettre la transmission de transaction en utilisant uniquement un système basé sur les ids, un mécanisme semblable au DNS (Domain Name Server) est mis en place.
  Le type dns_translation fait la traduction entre un id d'un noeud et une adresse internet représenté par le couple (IP, port).
  Un id est associé à un unique couple (IP, port).

  Pour déterminer à quelle noeud appartiennent les adresses de compte actuellement, chaque id est également associé à sa liste d'adresse. 
*)
type dns_translation = {
  id: int;
  internet_adress: Unix.inet_addr * int
  }


let string_of_dns_t dns_t =
  let ip, port = dns_t.internet_adress in
  "{id = " ^ string_of_int dns_t.id ^ ";adress = " ^ string_of_inet_addr ip ^ ":" ^ string_of_int port ^ "}"

(*
  Ce module permet de crée un ensemble de table de traduction DNS.
  Mauvais choix d'implémentation ce système d'id.
  Si j'avais le temps, je créerai plutôt un ensemble d'adresse (ip, port), bien plus simple.
*)
module DNS = Set.Make(
  struct
    type t = dns_translation
    let compare d1 d2 =
      let ip1, port1 = d1.internet_adress in
      let ip2, port2 = d2.internet_adress in
      if String.equal (string_of_inet_addr ip1) (string_of_inet_addr ip2) && port1 = port2 then
        0
      else if d1.id < d2.id then
        -1
      else
        1
  end
)

module AdressSet = Set.Make(
  struct
    type t = string
    let compare = String.compare
  end
)

let string_of_dns dns =
  DNS.fold (fun dns_t acc ->
    acc ^ "\n" ^ string_of_dns_t dns_t) dns ""


(*
  Les noeuds plein ou full node sont les seconds types de noeuds existant dans le système distribué. Les full nodes ont les mêmes propriétés que les 
  lazy nodes. De plus, les full nodes sont les garants de la bonne exécution du protocole de consensus du système distribué.

  Les noeuds plein accepte des transactions et sont chargé de les ajoutés à des blocs et de construire la blockchain. Ils sont également chargé de la transmission
  des blocs miné et de la blockchain la plus longue à tous les autres mineurs qu'il connaissent.

  les full nodes sont également responsable du maintient des tables de traductions du système DNS. Pour garantir l'intégrité d'une adresse, son ajout a 
  la table de traduction se fait via une transaction de la blockchain. Une adresse est ainsi considéré valide et est ajouté à la table de traduction 
  uniquement si elle est présente dans un bloc et que celui-ci existe depuis suffisament longtemps.
  Lorsqu'une adresse est accepté dans la blockchain, elle est associé avec le couple (IP, port) de la session. Une adresse peut changé de couple
  (IP, port) au cours du temps et ne permet pas de trouvé l'identité du propriétaire de l'adresse.

  Les full nodes sont également responsable du maintient des identifiants unique. Chaque noeud se connectant va demandé à recevoir un id.
  Ces ids sont incrémenté de 1 à chaque nouveau noeud s'ajoutant. Chaque noeud garde ainsi en mémoire l'id maximum qu'il a attribué via le
  champ max_id et délivre un nouvelle id, id + 1 au nouveau noeud s'ajoutant au système distribué.
*)
type full_node = {
  lazy_part: lazy_node;
  blockchain: block list;
  dns: DNS.t;
  current_coinbase: float
  }


let init_fullnode (ip, port) =
  {
    lazy_part = {
      id = 1;
      accounts = [];
      my_internet_adress = (ip, port);
      fullnode_info = None;
      connected_account = None
    };
    blockchain = [block_genesis];
    dns = DNS.empty;
    current_coinbase = 50.0
  }

  