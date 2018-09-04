open Str;;
(*#load "str.cma";;*)

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

(*Fonction pour fermer le cannal d'entrée in_channel*)
let fermerChannel canal =
  try
    close_in canal                (*fermer le canal donné*)
  with e ->                       (*exception car close_in ne déclanche pas l exception Sys_error lorsqu'elle est appliquée sur un canal d'entré fermé*)
    close_in_noerr canal;
raise e;;


(*Fonction pour calculer l'union de deux states*)
let union state1 state2 = let union1 = concate state1 state2 in
                          suppDoublons union1;;

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

(*Fonction pour vérifier si une knowlege structure est un knowledge space tq:
kst est knowledge space si elle est u-closed *)
let rec estKSP kst1 kst2 kst3= match kst1,kst2 with
  |[],[] -> true
  |[],kst2 -> false
  |kst1,[] -> false 
  |state::[],kst2 -> estKSP (List.tl kst2) (List.tl kst2) kst3
  |state::kst1Queue,kst2 ->  if (stateEstPresent (union state (List.hd kst1Queue))  kst3 = true) then estKSP (state::(List.tl kst1Queue)) kst2 kst3
                             else false;;

(*Fonctions d'affichage de listes*)
let rec afficherCharListe liste = match liste with 
  |[] -> ()
  |elt::listeQueue ->  Printf.printf "%C" elt ; Printf.printf " " ; afficherCharListe listeQueue;;

let rec afficherlisteListes liste = match liste with
  |[] -> ()
  |state::listeQueue -> Printf.printf "%s" "["; afficherCharListe state; Printf.printf "%s" "]"; afficherlisteListes listeQueue;; 

let afficherCharListeListe liste = Printf.printf "%s" "["; afficherlisteListes liste; Printf.printf "%s" "]";;

(*Fonction pour comparer la longueur de deux listes*)
let comparerLong l1 l2 = compare (List.length l1)(List.length l2);;

(*Fonction pour trier une liste de listes selon l'ordre croissant des tailles des sous-listes*)
let trierParTaille listeListes = List.sort comparerLong listeListes;;

(*Fonction pour vérifier l'inclusion d'une liste dans une autre liste*)
let rec inclusion state1 state2 = match state1 with
  |[] -> true
  |elt::state1Queue -> if (estPresent elt state2 = true ) then inclusion state1Queue state2
                       else false;;

(*Fonctions pour l'Algo de création de la base*)
(*Dans l'Algo de construction de base : 
* -> 1; 
- -> 0; 
+ -> 2; 
 *)

(*Fonction pour initialiser la matrice des Atoms avec: 
0 si qj n'appartient pas à state_i; 1 sinon *)
let initialiserMatriceAtoms matrice n m itemsArray statesArray=
  for i=0 to n-1 do
    for j=0 to m-1 do
      if (estPresent itemsArray.(j) statesArray.(i) = true) then matrice.(i).(j)<- 1
      else matrice.(i).(j)<- 0
    done;
  done;
  matrice;;

(*Fonction pour calculer Atoms avec: 
2 si qj appartient à state_i et pour un p : 1<=p<i qj appartient à state_p avec state_p inclu dans state_i *)
let traitementMatriceAtoms matrice n m itemsArray statesArray =
  for i=0 to n-1 do
    for j=0 to m-1 do
      if (matrice.(i).(j) = 1)then
        for p=0 to i-1 do
          if (matrice.(p).(j) = 1 && inclusion statesArray.(p) statesArray.(i) = true) then matrice.(i).(j)<- 2
        done;
    done;
  done;
  matrice;;


(*Fonction pour récuperer les states dont dans leur ligne on a au moins un 1*)
let atomsTableInit matriceAtoms n m checkArray statesArray =
  for i=0 to n-1 do
    for j=0 to m-1 do
      if (matriceAtoms.(i).(j)=1) then checkArray.(i)<-statesArray.(i)
    done;
  done;
  checkArray;;


(*Fonction pour récuperer la base = l'ensemble des atoms en éliminant les []*)
let rec recupererBase atomsListe = match atomsListe with
      |[]-> []
      |[]::baseQueue-> recupererBase baseQueue
      |elt::baseQueue -> elt::(recupererBase baseQueue);;

(*---------------PRETRAITEMENT---------------------------------------------------------------*)

(*Lire la famille d'ensemble depuis un fichier.txt*)
let fichier = "Knowledge_Structure_input.txt" ;;
let ic = open_in fichier;;    (*ouverture du in_channel pour lire depuis le fichier*)
let ligne = input_line ic;;   (*lire la premiere ligne du fichier*)
fermerChannel ic;;

print_string "\n";;
print_string "\n";;
print_string "ALGORITHME DE CONSTRUCTION DE LA BASE D'UN KNOWLEDGE SPACE";;
print_string "\n";;

print_string "\n";;
print_string "La famille d'ensembles K récupérée du fichier est : \n";;
let familleEns = stringToCharListeListes ligne;;
print_string "K = ";;
afficherCharListeListe familleEns;;

print_string "\n";;
print_string "La knowledge structure construite à partir de k est : \n";;
let kstructure= construireKST familleEns;;
print_string "(K,Q) = ";;
afficherCharListeListe kstructure;;

print_string "\n";;
print_string "Son domaine Q est : \n";;
let domaine = unionTousStates kstructure;;
print_string "Q = ";;
afficherCharListe domaine;;

print_string "\n";;
print_string "(K,Q) est un Knowledge space (càd elle est u-closed) ? (true= oui/ false = non) : \n";;
let kstEstKspace = estKSP kstructure kstructure kstructure;;
Printf.printf "Réponse :  %B" kstEstKspace;;




(*------ALGO DE CONSTRUCTION DE LA BASE DE KSpace------------------*)

(*1. lister arbitrairement les items q1,...,qm*)
let domaineQ= List.sort compare domaine;;

(*2. Lister les states de de (Q,K) comme suit: k1,...,kn tq si ki inclu dans kh alors i<h avec i,h dans {1..n}*)
let kspaceK= trierParTaille kstructure;;

(*3. Former un Array T (nxm)*)
let items = Array.of_list domaineQ;;
let m= Array.length items;;
let states = Array.of_list kspaceK;;
let n= Array.length states;;

let atomsInit = Array.make_matrix n m 0;;

(*4. Initialiser la matrice des atoms *)
let atomsInitialisee = initialiserMatriceAtoms atomsInit n m items states;;

(*5. Calculer les atoms*)
let atoms = traitementMatriceAtoms atomsInitialisee n m items states;;


(*initialiser un tableau avec des sous-listes vides*)
let checkArray = Array.make (Array.length states) [];;

(*Recuperer les state dont on a 1 dans leur ligne*)
let atomsTable = atomsTableInit atoms n m checkArray states;;
let atomsListe = Array.to_list atomsTable ;;


print_string "\n";;
print_string "La base du Knowledge Space est : \n";;
let base = recupererBase atomsListe;;
print_string "B = ";;
afficherCharListeListe base;;
print_string "\n";;
print_string "\n";;
