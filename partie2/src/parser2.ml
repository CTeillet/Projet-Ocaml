(*
   Ast : analyseur lexical et syntaxique des formules du tableur
         + construction de l'environnement de calcul des primitives
 *)

(* utilisation des streams et d'une grammaire descendante :

expr ::= atom suite_expr |  op(args)   |  epsilon
args ::= col1,lig1,col2,lig2 |  expr suite_args
suite_args ::= , expr | epsilon
suite_expr ::= + atom | - atom | * atom | / atom | epsilon
atom ::= int | float | string | #(col, lig) | (expr) | op1(expr)

où +,-,*,/ sont des opérations binaires
op est  une opération unaire, binaire ou une réduction
col : une suite de lettres
lig : un nombre entier

*)

open Tableur
open Stream
open Genlex

(* l'analyseur lexicale  *)

let keywords = ["+"; "-"; "*"; "/"; "("; ")"; "#"; ","]

let line_lexer l = Genlex.make_lexer keywords (Stream.of_string l)

(* calcul de la coordonnée de la colonne *)

let to_int s =
  let l = String.length s in
  let codeA = int_of_char 'A' in
  let r = ref 0 in
  let i = ref 0 in
  while !i < l do
    r := int_of_char s.[!i] - codeA + 1 + (26 * !r) ;
    i := !i + 1
  done ;
  !r

(* environnement des opérations unaires *)

let abs_r r =
  match r with
  | REntier i ->
      if i < 0 then REntier (-i) else r
  | RFlottant f ->
      if f < 0.0 then RFlottant (-.f) else r
  | _ ->
      Erreur Mauvais_arguments

let env_unaire = ref [("abs", abs_r)]

let creer_unaire op1 e1 =
  Unaire {app1 = List.assoc op1 !env_unaire; operande = e1}

(* environnement des opérations binaires *)

let make_opbin opint opfloat r1 r2 =
  match (r1, r2) with
  | (REntier i1, REntier i2) ->
      REntier (opint i1 i2)
  | (RFlottant f1, RFlottant f2) ->
      RFlottant (opfloat f1 f2)
  | (REntier i1, RFlottant f2) ->
      RFlottant (opfloat (float_of_int i1) f2)
  | (RFlottant f1, REntier i2) ->
      RFlottant (opfloat f1 (float_of_int i2))
  | _ ->
      Erreur Mauvais_arguments

let plus_r r1 r2 = make_opbin ( + ) ( +. ) r1 r2

let sous_r r1 r2 = make_opbin ( - ) ( -. ) r1 r2

let mult_r r1 r2 = make_opbin ( * ) ( *. ) r1 r2

let div_r r1 r2 = make_opbin ( / ) ( /. ) r1 r2

let env_binaire =
  ref
    [("add", plus_r); ("+", plus_r); ("-", sous_r); ("*", mult_r); ("/", div_r)]

let creer_binaire op e1 e2 =
  Binaire {app2 = List.assoc op !env_binaire; gauche = e1; droite = e2}

let somme_r = plus_r

let env_reduction = ref [("somme", (somme_r, REntier 0))]

let creer_reduction op i1 i2 i3 i4 =
  let (app_r, acc) = List.assoc op !env_reduction in
  Reduction
    {app = app_r; init = acc; case_debut = (i1, i2); case_fin = (i3, i4)}

(* la grammaire *)

exception Bad_op of string

exception Bad_atom

exception Bad_case

exception Bad_token

let kwd tok v =
  match tok with
  | Kwd m2 ->
      if m2 != v then raise Bad_token
  | _ ->
      raise Bad_token

let int tok = match tok with Int i -> i | _ -> raise Bad_token

let ident tok = match tok with Ident id -> id | _ -> raise Bad_token

let rec atom s =
  match next s with
  | String s ->
      Chaine s
  | Int i ->
      Entier i
  | Float f ->
      Flottant f
  | Kwd "#" ->
      let (id, i) = case s in
      Case (to_int id, i - 1)
  | Kwd "(" ->
      let e = expr s in
      kwd (next s) ")" ;
      e
  | _ ->
      raise Bad_atom

and case s =
  try
    kwd (next s) "(" ;
    let id = ident (next s) in
    kwd (next s) "," ;
    let i = int (next s) in
    kwd (next s) ")" ;
    (id, i)
  with e -> raise Bad_case

and suite_expr e1 s =
  try
    match next s with
    | Kwd op ->
        if List.mem op ["+"; "-"; "*"; "/"] then
          let e2 = atom s in
          creer_binaire op e1 e2
        else raise (Bad_op op)
    | _ ->
        raise Bad_token
  with Failure -> e1

and args op s =
  match next s with
  | Ident id1 ->
      kwd (next s) "," ;
      let i2 = int (next s) in
      kwd (next s) "," ;
      let id3 = ident (next s) in
      kwd (next s) "," ;
      let i4 = int (next s) in
      creer_reduction op (to_int id1) i2 (to_int id3) i4
  | _ ->
      let e1 = expr s in
      let e2 = suite_args op e1 s in
      e2

and suite_args op e1 s =
  try
    match next s with
    | Kwd "," ->
        let e2 = expr s in
        creer_binaire op e1 e2
    | _ ->
        raise Bad_token
  with Failure -> creer_unaire op e1

and expr s =
  try
    match next s with
    | Ident op ->
        kwd (next s) "(" ;
        let e = args op s in
        kwd (next s) ")" ;
        e
    | _ ->
        let e1 = atom s in
        let e = suite_expr e1 s in
        e
  with Failure -> Vide

(* la fonction de création de l'ast à partir d'une chaîne de caractères *)

let make_ast s =
  try Ok (expr (line_lexer s)) with
  | Bad_op s ->
      Error ("bad op : " ^ s)
  | Bad_atom ->
      Error "bad atom"
  | Bad_case ->
      Error "bad cell"
  | Bad_token ->
      Error "bad token"

(* exemples

make_ast "#(AA,12)";;
make_ast "somme(AB,10,AD,14)" ;;
make_ast "abs(2 + 4)" ;;
make_ast "add(3 * 4, 2 - 3)";;
make_ast "add( (abs(3 - 4 )), (somme(AB,10,AD,14)))";;

attention "-3" est considéré comme un entier  par le lexer
et donc "2-3" est vu comme la suité de 2 entiers 2 et -3
il faut alors ecrire "2 - 3" et c'est bien l'opérateur "-"

make_ast "moyenne(AB,10,AD,14)" ;;
make_ast "oppose(3)" ;;

pour le moment il n'y a pas beaucoup de gestion d'erreurs
s elles sont assez simple à gérer :

  syntaxe
  fonction non définie

*)
