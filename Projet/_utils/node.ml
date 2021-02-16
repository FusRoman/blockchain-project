open Cryptokit
open Unix

(* 
  Un compte est une collection de transaction envoyé ou reçu à l'adresse qui appartient au compte.

  Chaque compte contient une liste de transaction confirmé, elle appartiennent à un bloc de la blockchain et ce bloc existe depuis 
  suffisamment longtemps.
  La liste des transactions non confirmé contient quand à elle des transactions n'ayant soit pas été intégré à un bloc soit ayant été intégré dans un bloc mais celui
  ci n'existe pas depuis suffisament longtemps.

  Chaque compte est associé à un pair de clé privé/public pour garantir la sécurité du compte via le protocole RSA.
  Un compte a également un nom donné par l'utilisateur pour la commodité.

  Le compte ne contient pas d'argent bien qu'une variable euc (Earth Unified Currency) existe. Celle ci est juste un indicateur de nombre de wuc associé à cette adresse 
  après le parcours des transactions confirmé dans la blockchain.
*)
type account = {account_name : string; euc: Z.t; rsa_key: RSA.key; adress: string; mutable confirmed_transaction: Z.t list; mutable unconfirmed_transaction: Z.t list}


(*
  Chaque transaction contient un identifiant id unique à toute les transactions.
  Un transaction est un flux de cryptomonnaie entre un emetteur et un recepteur symbolisé par leurs adresses.
  La quantité d'argent transféré est symbolysé par le champ amount.

  Pour qu'une transaction soit accepté dans la blocchain, un mineur doit l'ajouté. Pour compensé la dépense énergétique d'un mineur, une quantité
  d'argent lui est versé via le champ few.
  La difficulté de minage d'un bloc est directement relié à la valeur few que recevra le mineur. Plus few est élevé, plus le bloc sera difficile a miné.
*)
type transaction = {id: Z.t; sender_adress: string; receiver_adress: string; amount: Z.t; few: Z.t}


(*
  Deux types de noeuds existe dans le système distribué.
  Un lazy node est un noeud uniquement capable de faire des transactions. Il est identifié par un id unique à tous les noeuds (qu'il soit lazy ou full) 
  du système distribué.
*)
type lazy_node = {id: Z.t; accounts: account list}


(*
  Pour permettre la transmission de transaction en utilisant uniquement un système d'adresse, un mécanisme semblable au DNS (Domain Name Server) est mis en place.
  Le type dns_translation fait la traduction entre une adresse d'un compte représenté par une chaine de caractère et une adresse internet 
  représenté par le couple (IP, port).
  Une adresse est unique mais un même couple (IP, port) peut être associé à plusieurs adresses. 
*)
type dns_translation = {adress: string; mutable internet_adress: Unix.inet_addr * int}


(*
  Les noeuds plein ou full node sont les seconds types de noeuds existant dans le système distribué. Les full nodes ont les mêmes propriétés que les 
  lazy nodes. De plus, les full nodes sont les garant de la bonnes exécutions du protocole de consensus du système distribué.

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
type full_node = {lazy_part: lazy_node; dns: dns_translation list; max_id: Z.t}


  