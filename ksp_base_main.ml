#load "kst.cmo";;
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

(*Retourner le domaine Q*)
let domaine = Kst.unionTousStates kst;;

(*Verifier si KST k est un Knowledge space KSP*)
(*Fonction pour vérifier si une knowlege structure est un knowledge space*)
let rec estKSP kstQ kst = match kstQ,kst with
  |[],[] -> true
  |[],kst -> false
  |kstQ,[] -> false 
  |state::[],kst -> estKSP (List.tl kst) (List.tl kst)
  |state::kstQueue,kst -> let unionV1 =Kst.concate state (List.hd kstQueue) in
                          let unionV2 = Kst.suppDoublons unionV1 in
                          if (Kst.stateEstPresent unionV2 kst = true) then estKSP (state::(List.tl kstQueue)) kst
                          else false;;

let estKsp = estKSP kst kst;;






