(** Types *)

type expr =
  | Vide
  | Chaine of string
  | Entier of int
  | Flottant of float
  | Case of int * int

type resultat =
  | RVide
  | RChaine of string
  | REntier of int
  | RFlottant of float
  | Erreur of erreur

and erreur = 
  |Mauvais_indice of (int * int)
  |Cycle_detecte of (int * int)

type grille = expr array array

(** Fonctions *)

exception Pas_encore_implemente of string

let cree_grille i j  = 
    Array.make_matrix i j Vide

let expr_to_string expr = 
    match expr with 
    |Vide -> ""
    |Entier i -> string_of_int i
    |Flottant i-> string_of_float i
    |Chaine  s-> s
    |Case (i, j) -> "@("^(string_of_int i)^","^(string_of_int j)^")"


let affiche_grille (gr:grille) = 
    Array.iter (
      fun i -> Array.iter (
        fun j -> Format.printf "|%8s|" (expr_to_string j)) i;print_newline()) gr

let cycle (gr:grille) (ex:expr) = 
    let listCase = ref [] in
    let rec testCycle e = 
      match e with 
      | Case (i, j) as t ->
                    if List.mem t !listCase then  
                      true
                    else (
                      listCase := !listCase @ [t] ;
                      testCycle gr.(i).(j))
      |_ -> false
      in
    testCycle ex
  
let rec eval_expr (grille : grille) (expr : expr) = 
    match expr with
    |Case (i, j) ->  if i >= Array.length grille || j>=Array.length grille.(0) then 
                            if not (cycle grille expr) then
                              eval_expr grille grille.(i).(j)
                            else
                              Erreur (Cycle_detecte(i,j))
                          else
                            Erreur (Mauvais_indice (i,j))
    |Entier i -> REntier i
    |Vide -> RVide
    |Flottant i -> RFlottant i
    |Chaine c -> RChaine c

let cree_grille_resultat i j  = 
    Array.make_matrix i j RVide

let eval_grille (grille : grille) =
    let res = cree_grille_resultat (Array.length grille) (Array.length grille.(0)) in
    for i=0 to Array.length res -1 do
      for j=0 to Array.length res.(0) -1 do
        res.(i).(j) <- eval_expr grille grille.(i).(j)
      done;
    done;
    res
    

let r = cree_grille 10 10;;
r.(1).(1) <- Entier(1);;
r.(1).(2) <- Case(2, 1);;
r.(2).(1) <- Case(1, 4);;
r.(1).(4) <- Case(5, 5);;
r.(5).(5) <- Case(6, 5);;
r.(6).(5) <- Case(1, 2);;
print_string (string_of_bool (cycle r (Case(1, 2))));;
affiche_grille r;;

