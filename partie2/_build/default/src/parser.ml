open Angstrom
open Tableur

(** BNF

<vide> est une case vide
<stringlit> est une chaine quelconque
<intlit> est un entier litéral
<letter> est une expression de coordonnée de colonne (A, AA, BA)

<case> :=
| <stringlit>
| <intlit>
| = <operation>
| <vide>

<operation> :=
| <operation> + <operation>
| <operation> - <operation>
| <operation> * <operation>
| <operation> / <operation>
| #(<letter>, <intlit>)

*)

let letter_to_int s =
  let rec pow x n = if n = 0 then 1 else x * pow x (n - 1) in
  let idmax = String.length s - 1 in
  (fun n -> n - 1)
  @@ fst
  @@ Seq.fold_left
       (fun (acc, i) c ->
         let n = int_of_char c - int_of_char 'A' + 1 in
         ((n * pow 26 i) + acc, i - 1))
       (0, idmax)
  @@ String.to_seq s

module type IMPLEM = sig
  val add : resultat -> resultat -> resultat

  val sub : resultat -> resultat -> resultat

  val mul : resultat -> resultat -> resultat

  val div : resultat -> resultat -> resultat
end

module Make (Implem : IMPLEM) = struct
  let token p = p <* many @@ char ' '

  let char_token c = char c <* many @@ char ' '

  let parens p = char_token '(' *> p <* char_token ')'

  let add =
    char_token '+'
    *> return (fun gauche droite ->
           Binaire {app2 = Implem.add; gauche; droite})

  let sub =
    char_token '-'
    *> return (fun gauche droite ->
           Binaire {app2 = Implem.sub; gauche; droite})

  let mul =
    char_token '*'
    *> return (fun gauche droite ->
           Binaire {app2 = Implem.mul; gauche; droite})

  let div =
    char_token '/'
    *> return (fun gauche droite ->
           Binaire {app2 = Implem.div; gauche; droite})

  let int_lit =
    take_while1 (function '0' .. '9' -> true | _ -> false)
    |> token >>| int_of_string

  let integer = int_lit >>| fun i -> Entier i

  let cell_ident =
    take_while1 (function 'A' .. 'Z' -> true | _ -> false) |> token

  (* #(A, 5) *)

  let cell =
    char_token '#' *> commit *> char '(' *> cell_ident
    >>= fun s ->
    char_token ',' *> int_lit
    <* char_token ')'
    >>| fun i -> Case (i - 1, letter_to_int s)

  let chainl1 e op =
    let rec go acc = lift2 (fun f x -> f acc x) op e >>= go <|> return acc in
    e >>= fun init -> go init

  let op_expr =
    many (char ' ')
    *> fix (fun expr ->
           let factor = parens expr <|> integer <|> cell in
           let term = chainl1 factor (mul <|> div) in
           chainl1 term (add <|> sub))
    <* end_of_input <|> fail "not an op"

  let op = char_token '=' *> commit *> op_expr

  let expr =
    end_of_input
    >>| (fun () -> Vide)
    <|> op <|> (integer <* end_of_input)
    <|> (take_while (fun _ -> true) >>| fun s -> Chaine s)

  let eval (str : string) = parse_string expr str
end
