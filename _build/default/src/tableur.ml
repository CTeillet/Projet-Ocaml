(** Types *)

type resultat =
  | RVide
  | RChaine of string
  | REntier of int
  | RFlottant of float
  | Erreur of erreur
and erreur =
  |Mauvais_indice of (int * int)
  |Cycle_detecte of (int * int)

type expr =
  | Vide
  | Chaine of string
  | Entier of int
  | Flottant of float
  | Case of int * int
  | Unaire of op_unaire
  | Binaire of op_binaire
  | Reduction of op_reduction

and op_unaire = {app1 : resultat -> resultat ; operande : expr}
and op_binaire = {app2 : resultat -> resultat -> resultat ; gauche : expr ; droite : expr}
and op_reduction = {app : resultat -> resultat -> resultat; init : resultat ; case_debut : int * int; case_fin : int * int }

type grille = expr array array

type grille_resultat = resultat array array

(** Fonctions *)

exception Pas_encore_implemente of string
exception Cycle_detecte


module CaseSet = Set.Make(struct
  type t = int*int
  let compare = compare
end)

let cree_grille i j  =
    Array.make_matrix i j Vide

let res_to_string expr =
    match expr with
    |RVide -> ""
    |REntier i -> string_of_int i
    |RFlottant i-> string_of_float i
    |RChaine  s-> s
    |Erreur e-> match e with
      |Mauvais_indice (i,j) -> "Mauvais indice " ^"("^string_of_int i^","^string_of_int j^")"
      |Cycle_detecte (i,j) -> "Cycle Present"^"("^string_of_int i^","^string_of_int j^")"

let rec expr_to_string expr =
    match expr with
    |Vide -> ""
    |Entier i -> string_of_int i
    |Flottant i-> string_of_float i
    |Chaine  s-> s
    |Case (i, j) -> "@("^(string_of_int i)^","^(string_of_int j)^")"
    |Unaire e -> "u("^(expr_to_string e.operande)^")"
    |Binaire b -> "b("^(expr_to_string b.gauche)^","^(expr_to_string b.droite)^")"
    |Reduction r -> "r("^(expr_to_string (Case(fst r.case_debut, snd r.case_debut)))^","^(expr_to_string (Case(fst r.case_fin, snd r.case_fin)))^")"

let affiche_grille (gr:grille) =
    Array.iter (
      fun i -> Array.iter (
        fun j -> Format.printf "|%8s|" (expr_to_string j)) i;print_newline()) gr

let cycle (gr:grille) (ex:expr) =
    let ensemble_case = ref CaseSet.empty in
    let rec testCycle e =
      match e with
      | Case (i, j) ->
                    if CaseSet.mem (i,j) !ensemble_case then
                      true
                    else (
                      ensemble_case := CaseSet.add (i,j) !ensemble_case;
                      testCycle gr.(i).(j))
      |Unaire u -> testCycle u.operande
      |Binaire b -> testCycle b.gauche || testCycle b.droite
      |Reduction r -> (try 
                        for i=(fst r.case_debut) to (fst r.case_fin) do 
                          for j=(snd r.case_fin) to (snd r.case_fin) do 
                              if CaseSet.mem (i,j) !ensemble_case then
                                raise Cycle_detecte
                              else 
                                ensemble_case := CaseSet.add (i,j) !ensemble_case;
                                let t = testCycle gr.(i).(j) in
                                if t then
                                  raise Cycle_detecte 
                          done
                        done;
                        false
                      with
                        | Cycle_detecte -> true
                       )
      |_ -> false
      in
    testCycle ex

let rec eval_expr (grille : grille) (expr : expr) =
    match expr with
    |Case (i, j) ->  if i >= Array.length grille || j>=Array.length grille.(0) then
                            Erreur (Mauvais_indice (i,j))
                          else
                            if not (cycle grille expr) then
                              eval_expr grille grille.(i).(j)
                            else
                              Erreur (Cycle_detecte(i,j))
    |Entier i -> REntier i
    |Vide -> RVide
    |Flottant i -> RFlottant i
    |Chaine c -> RChaine c
    |Unaire u -> u.app1 (eval_expr grille u.operande)
    |Binaire b -> b.app2 (eval_expr grille b.gauche) (eval_expr grille b.droite)
    |_ -> RVide

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

let affiche_grille_resultat (grille_res:grille_resultat) =
    Array.iter (
      fun i -> Array.iter (
        fun j -> Format.printf "|%8s|" (res_to_string j)) i;print_newline()) grille_res

(**let r = cree_grille 10 10;;
r.(1).(1) <- Entier(1);;
r.(1).(2) <- Case(2, 1);;
r.(2).(1) <- Case(1, 4);;
r.(1).(4) <- Case(5, 5);;
r.(5).(5) <- Case(6, 5);;
r.(6).(5) <- Case(1, 2);;
print_string (string_of_bool (cycle r (Case(1, 2))));;
affiche_grille r;;**)
