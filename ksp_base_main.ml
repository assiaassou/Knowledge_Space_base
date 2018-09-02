open Kst;;
(*Lire la famille d'ensemble depuis un fichier.txt*)
(*la famille d'ensembles doit etre ecrite dans la premiere ligne du fichier et les états doivent contetnir des caractères qui désignent les items, telque les items d'un meme state sont collés tandis que les states sont déparés par des virgules ','*)

let fichier = "Knowledge_Structure_input.txt" ;;
let ic = open_in fichier;;    (*ouverture du in_channel pour lire depuis le fichier*)
let ligne = input_line ic;;   (*lire la premiere ligne du fichier*)
Kst.fermerChannel ic;;

(*Construire la knowledge Structure à partir de la famille d'ensembles récupérée depuis le fichier*)
let kst=
  let familleEns = Kst.stringToCharListeListes ligne in
  Kst.construireKST familleEns;;
  







