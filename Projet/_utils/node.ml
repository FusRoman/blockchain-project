open Unix
open Block

(* 
  Un compte est une collection de transaction envoyé ou reçu à l'adresse qui appartient au compte.

  Chaque compte contient une liste de transaction. Chaque transaction contient l' id de la transaction, la date à laquelle elle a été confirmé, 
  une adresse et le montant de cryptomonnaie transféré par la transaction.
  L'adresse contenue dans la transaction est soit l'émetteur dans le cas où le compte a reçu de la monnaie ou le recepteur
  dans le cas où le wallet a envoyé de l'argent.

  Chaque compte est associé à une pair de clé privé/public pour garantir la sécurité du compte via le protocole RSA.
  Un compte a également un nom donné par l'utilisateur pour la commodité.

  Le compte ne contient pas d'argent bien qu'une variable euc_balance (Earth Unified Currency) existe. Celle ci est juste un indicateur de nombre de euc 
  associé à cette adresse après le parcours des transactions confirmé dans la blockchain.
*)
type account = {
  account_name : string;
  euc_balance: euc;
  rsa_key: Cryptokit.RSA.key;
  adress: string;
  mutable transaction: (int * Unix.tm option * string * euc) list
  }


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
  mutable fullnode_info: (int * (Unix.inet_addr * int)) option
  }


(*
  Pour permettre la transmission de transaction en utilisant uniquement un système basé sur les ids, un mécanisme semblable au DNS (Domain Name Server) est mis en place.
  Le type dns_translation fait la traduction entre un id d'un compte et une adresse internet représenté par le couple (IP, port).
  Un id est associé à un unique couple (IP, port).
*)
type dns_translation = {
  id: int;
  mutable account_adress: string list;
  internet_adress: Unix.inet_addr * int
  }

  module DNS = Set.Make(
    struct
      type t = dns_translation
      let compare d1 d2 = 
        if d1.id = d2.id then
          0
        else if d1.id < d2.id then
          -1
        else
          0
    end
  )

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
  dns: DNS.t;
  max_id: Z.t
  }


  