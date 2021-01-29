# blockchain-project

La compilation du projet se fait en lancant la commande make dans une console.

Les exécutables sont dans le dossier _exec

Il est possible de lancer les exécutables depuis le make file, voici les commandes :

    -Pour lancer un mineur :
        make new_miner
    
    -Pour lancer la commande help d'un mineur :
        make new_miner ARGS='-help'
    
    -Pour lancer un mineur avec une adresse spécifique :
        make new_miner ARGS='-my_addr 127.0.0.1 8000'

    -Pour lancer un mineur qui se connecte à un autre mineur :
        make new_miner ARGS='my_addr 127.0.0.1 8000 -distant_ip 127.0.0.1 -distant_port 8001'
    
    -Pour lancer un waller:
        make new_waller ARGS='-miner_addr 127.0.0.1 8000'