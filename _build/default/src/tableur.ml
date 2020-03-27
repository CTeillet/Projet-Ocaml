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
  |Mauvais_argument of string

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

let type_res_to_string (e:resultat) =
  match e with 
    |RVide -> "RVide"
    |RChaine _ -> "RChaine"
    |REntier _ -> "REntier"
    |RFlottant _ -> "RFlottant"
    |Erreur _ -> "Erreur"


let res_to_string expr =
    match expr with
    |RVide -> ""
    |REntier i -> string_of_int i
    |RFlottant i-> string_of_float i
    |RChaine  s-> s
    |Erreur e-> match e with
      |Mauvais_indice (i,j) -> "Mauvais indice " ^"("^string_of_int i^","^string_of_int j^")"
      |Cycle_detecte (i,j) -> "Cycle Present"^"("^string_of_int i^","^string_of_int j^")"
      |Mauvais_argument s -> s

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

let abs (v:expr) =
  let f (r:resultat) = 
    match  r with 
      | REntier e -> REntier (abs e)
      | RFlottant f -> RFlottant (abs_float f)
      | _ -> Erreur (Mauvais_argument ("Attendus un entier ou flottant mais argument de type "^(type_res_to_string r))) 
  in
  let t = {app1=f; operande=v} in
  Unaire(t)

let add (a:expr) (b:expr)=
  let f (r:resultat) (s:resultat)= 
    match  (r,s) with 
      | (REntier e, REntier d) -> REntier (e+d)
      | (RFlottant f, RFlottant e) -> RFlottant (f+.e)
      | (RFlottant f, REntier e) -> RFlottant (f +. (float_of_int e))
      | (REntier e, RFlottant f) -> RFlottant (f +. (float_of_int e))
      | _ -> Erreur (Mauvais_argument ("Attendus un entier ou flottant mais argument de type "^(type_res_to_string r)^" et de type "^(type_res_to_string s))) 
  in
  let t = {app2=f; gauche=a; droite=b} in
  Binaire(t)

(*let somme (case_debut:(int*int)) (case_fin:(int*int)) =
  *)

let oppose (v:expr) =
  let f (r:resultat) = 
    match  r with 
      | REntier e -> REntier (-e)
      | RFlottant f -> RFlottant (-.f)
      | _ -> Erreur (Mauvais_argument ("Attendus un entier ou flottant mais argument de type "^(type_res_to_string r))) 
  in
  let t = {app1=f; operande=v} in
  Unaire(t)

let inverse (v:expr) =
  let f (r:resultat) = 
    match  r with 
      | REntier e -> REntier (1/e)
      | RFlottant f -> RFlottant (1./.f)
      | _ -> Erreur (Mauvais_argument ("Attendus un entier ou flottant mais argument de type "^(type_res_to_string r))) 
  in
  let t = {app1=f; operande=v} in
  Unaire(t)

let minus (a:expr) (b:expr)=
  let f (r:resultat) (s:resultat)= 
    match  (r,s) with 
      | (REntier e, REntier d) -> REntier (e-d)
      | (RFlottant f, RFlottant e) -> RFlottant (f-.e)
      | (RFlottant f, REntier e) -> RFlottant (f -. (float_of_int e))
      | (REntier e, RFlottant f) -> RFlottant (f -. (float_of_int e))
      | _ -> Erreur (Mauvais_argument ("Attendus un entier ou flottant mais argument de type "^(type_res_to_string r)^" et de type "^(type_res_to_string s))) 
  in
  let t = {app2=f; gauche=a; droite=b} in
  Binaire(t)

let mul (a:expr) (b:expr)=
  let f (r:resultat) (s:resultat)= 
    match  (r,s) with 
      | (REntier e, REntier d) -> REntier (e*d)
      | (RFlottant f, RFlottant e) -> RFlottant (f*.e)
      | (RFlottant f, REntier e) -> RFlottant (f *. (float_of_int e))
      | (REntier e, RFlottant f) -> RFlottant (f *. (float_of_int e))
      | _ -> Erreur (Mauvais_argument ("Attendus un entier ou flottant mais argument de type "^(type_res_to_string r)^" et de type "^(type_res_to_string s))) 
  in
  let t = {app2=f; gauche=a; droite=b} in
  Binaire(t)

let div (a:expr) (b:expr)=
  let f (r:resultat) (s:resultat)= 
    match  (r,s) with 
      | (REntier e, REntier d) -> REntier (e/d)
      | (RFlottant f, RFlottant e) -> RFlottant (f/.e)
      | (RFlottant f, REntier e) -> RFlottant (f /. (float_of_int e))
      | (REntier e, RFlottant f) -> RFlottant (f /. (float_of_int e))
      | _ -> Erreur (Mauvais_argument ("Attendus un entier ou flottant mais argument de type "^(type_res_to_string r)^" et de type "^(type_res_to_string s))) 
  in
  let t = {app2=f; gauche=a; droite=b} in
  Binaire(t)

let max (a:expr) (b:expr)=
  let f (r:resultat) (s:resultat)= 
    match  (r,s) with 
      | (REntier e, REntier d) -> REntier (max e d)
      | (RFlottant f, RFlottant e) -> RFlottant (max f e)
      | (RFlottant f, REntier e) -> RFlottant (max f (float_of_int e))
      | (REntier e, RFlottant f) -> RFlottant (max f (float_of_int e))
      | _ -> Erreur (Mauvais_argument ("Attendus un entier ou flottant mais argument de type "^(type_res_to_string r)^" et de type "^(type_res_to_string s))) 
  in
  let t = {app2=f; gauche=a; droite=b} in
  Binaire(t)

let min (a:expr) (b:expr)=
  let f (r:resultat) (s:resultat)= 
    match  (r,s) with 
      | (REntier e, REntier d) -> REntier (min e d)
      | (RFlottant f, RFlottant e) -> RFlottant (min f e)
      | (RFlottant f, REntier e) -> RFlottant (min f (float_of_int e))
      | (REntier e, RFlottant f) -> RFlottant (min f (float_of_int e))
      | _ -> Erreur (Mauvais_argument ("Attendus un entier ou flottant mais argument de type "^(type_res_to_string r)^" et de type "^(type_res_to_string s))) 
  in
  let t = {app2=f; gauche=a; droite=b} in
  Binaire(t)

let sqrt (v:expr) =
  let f (r:resultat) = 
    match  r with 
      | REntier e -> REntier (sqrt (float_of_int e))
      | RFlottant f -> RFlottant (sqrt f)
      | _ -> Erreur (Mauvais_argument ("Attendus un entier ou flottant mais argument de type "^(type_res_to_string r))) 
  in
  let t = {app1=f; operande=v} in
  Unaire(t)