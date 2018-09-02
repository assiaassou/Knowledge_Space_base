#load "str.cma";;
(*Fonction calcule le nombre de states dans la knowledge structure*)
let rec nbrStates liste = match liste with
  |[] -> 0
  |elt::listeQueue -> 1 + (nbrStates listeQueue);;


(*Fonction vérifie si un element x est présent dans une liste*)
let rec estPresent x liste = match liste with
  |[] -> false
  |elt::[] -> if (elt = x) then true
             else false
  |elt::listeQueue -> if (elt = x) then true
                                          else estPresent x listeQueue;;

(*Fonction supprime les doublons dans une liste*)
let rec suppDoublons liste = match liste with
  |[] -> []
  |elt::[] -> liste
  |elt::listeQueue -> if (estPresent elt listeQueue = true) then suppDoublons listeQueue
                      else elt::suppDoublons listeQueue;;



(*Fonction ajoute un element à une liste tout en vérifiant qu'il n'existe pas*)
let ajouterElt elt liste = match liste with
  |[] -> [elt]
  |e::listeQueue -> if (estPresent elt liste = true)
                    then liste
                    else elt::liste;;



(*Fonction pour comparer deux states S1 S2 qui ont la meme cardinalité*)
let rec comparerStates l1 l2 = match l1, l2 with
       |[], [] -> true
       |[], _ -> true
       |e1::l1Queue, _ -> if (estPresent e1 l2 = true) then comparerStates l1Queue l2
                          else false;;


(*Fonction verifie si deux states ont la meme cardinalité pour les comparer*)
let statesSontComparables l1 l2= if (nbrStates l1 = nbrStates l2 )
                                then comparerStates l1 l2
                                else false;;


(*Fonction vérifie si un state K est présent dans une liste*)
let rec  stateEstPresent stateK listeListes = match listeListes with
  |[] -> false
  |state1::[] -> if (statesSontComparables state1 stateK = true) then true
             else false
  |state1::listeListesQueue -> if (statesSontComparables stateK state1 = true) then true
                                          else stateEstPresent stateK listeListesQueue;;


(*Fonction supprime les states doublons*)
let rec suppStatesDoublons listeListes = match listeListes with
  |[] -> []
  |state::[] -> listeListes
  |state1::listeListesQueue -> if (stateEstPresent state1 listeListesQueue = true) then suppStatesDoublons listeListesQueue
                               else state1::suppStatesDoublons listeListesQueue;;


(*Fonction concatener deux states*)
let rec concate state1 state2 = match state1 with
  |[] -> state2
  |item::state1Queue -> item::(concate state1Queue state2);;


(*Fonction retourne l'union de tous les states*)
let rec unionTousStates listeListes = match listeListes with
  |[]->[]
  |state::[] -> suppDoublons state
  |state::listeListesQueue -> unionTousStates ((concate state (List.hd listeListesQueue))::(List.tl listeListesQueue));;


(*Fonction pour construire la Knowledge Structure comme suit:
1.Vérifier si la famille d'ensembles a une taille >= 2 
2.Supprimer les items doublons dans chaque state
3.Supprimer les states doublons
4.ajouter l'ensemble vide et l'union de tous les ensembles d'ils n'existent pas
*)
let construireKST listeListes = if (nbrStates listeListes < 2) then failwith "La Knowledge Structure doit contenir au moins deux States (l'ensemble vide et le domaine)!"
                                else let listeListesV2 = List.map suppDoublons listeListes in
                                     let listeListesV3 =  suppStatesDoublons listeListesV2 in
                                     let listeListesV4 = ajouterElt [] listeListesV3 in
                                     ajouterElt (unionTousStates listeListesV4) listeListesV4;;


(*Fonction pour convertir une chaine de char en char liste *)
let explode s =
  let rec exp i l = if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) [];;


(*Fonction pour convertir une liste de strings en liste de char liste*)
let rec stringToCharListe listeStrings= match listeStrings with
  |[] ->[]
  |elt::[] -> (explode elt)::[]
  |elt::listeStrings -> (explode elt)::(stringToCharListe listeStrings);;

(*Fonction pour convertir une chaine de char en liste de char listes*)
let stringToCharListeListes ligne = let famille =  Str.split (Str.regexp_string ",") ligne in
                                    stringToCharListe famille;;

(*Fonction pour fermer le cannal d'entrér in_channel*)
let fermerChannel canal =
  try
    close_in canal                (*fermer le canal donné*)
  with e ->                       (*exception car close_in ne déclanche pas l exception Sys_error lorsqu'elle est appliquée sur un canal d'entré fermé*)
    close_in_noerr canal;
raise e;;

(*-----------------KSP----------------*)

(*Fonction pour vérifier si une knowlege structure est un knowledge space*)
let rec estKSP kstQ kst = match kstQ,kst with
  |[],[] -> true
  |[],kst -> false
  |kstQ,[] -> false 
  |state::[],kst -> estKSP (List.tl kst) (List.tl kst)
  |state::kstQueue,kst -> let unionV1 = concate state (List.hd kstQueue) in
                          let unionV2 = suppDoublons unionV1 in
                          if (stateEstPresent unionV2 kst = true) then estKSP (state::(List.tl kstQueue)) kst
                          else false;;
