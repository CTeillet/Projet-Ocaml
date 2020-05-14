module Implem = struct end

open Parser
open Js_utils

let ( -- ) i j = List.init (j - i + 1) (fun x -> x + i)

type cell_infos = {
  container : Dom.div;
  inp : Dom.input;
  txt : Dom.txt;
  mutable result : Tableur.resultat;
  mutable parent_deps : (int*int) list;
  mutable child_deps : (int*int) list;
}

exception Mauvais_Format of string

let direct_deps expr =
  let rec aux expr =
    let open Tableur in
    match expr with
    | Vide | Entier _ | Flottant _ | Chaine _ ->
        CaseSet.empty
    | Case (i, j) ->
        CaseSet.singleton (i, j)
    | Unaire {operande; _} ->
        aux operande
    | Binaire {gauche; droite; _} ->
        CaseSet.union (aux gauche) (aux droite)
    | Reduction {case_debut; case_fin; _} ->
        List.fold_left (fun set c -> CaseSet.add c set) CaseSet.empty
        @@ coords_of_plage case_debut case_fin
  in
  Tableur.CaseSet.elements (aux expr)

type grid = Tableur.expr array array

type infos_grid = cell_infos array array

let mk_cell ?(inp = Dom.Create.input ()) ?(container = Dom.Create.div ())
    ?(txt = Dom.Create.txt " ") ?(result = Tableur.RVide) ?(parent_deps = []) ?(child_deps = [])() =
  {inp; container; txt; result; parent_deps; child_deps}

let error_to_string e = assert false (* TODO *)

let resultat_to_string r = (* Question 12 *)
  Tableur.res_to_string r

let update_display (infos_grid:infos_grid) i j (r:Tableur.resultat) = (* Question 13 *)
  match r with
  | Tableur.Erreur e-> Dom.Class.add infos_grid.(i).(j).container "cell-error"
  | _ -> let chaine = (resultat_to_string r) in
         Dom.Text.set_content infos_grid.(i).(j).txt chaine;
         Dom.Class.remove infos_grid.(i).(j).container "cell-error"
  
let update_deps (infos_grid:infos_grid) (i:int) j (expr:Tableur.expr) =
  let parent_deps = direct_deps expr in
  infos_grid.(i).(j).parent_deps <-infos_grid.(i).(j).parent_deps @ parent_deps;
  let p c =  if (((fst c) = i) && ((snd c) = j)) then true else false in
  Array.iteri (fun k a -> Array.iteri (fun l b -> if (List.exists p b.parent_deps ) then infos_grid.(i).(j).child_deps <-infos_grid.(i).(j).child_deps @ [(k,l)] ) a) infos_grid

let propagate grid infos_grid i j = assert false (* TODO *)

let grid_to_string grid infos_grid = (* Question 4 *)
  let res = ref [] in
  Array.iteri (fun i e -> Array.iteri (fun j f -> if Dom.Input.get_value (infos_grid.(i).(j).inp) != "" then 
                                                    res := (Printf.sprintf "%d|%d|%s" i j  (Dom.Input.get_value f.inp))::!res) e)   infos_grid;
  String.concat "\n" !res

let cells_of_string storage_grid = (* Question 5 *)
  let r = String.split_on_char '\n' storage_grid in
  let res = ref [] in
  List.iter (fun (i:string) ->  let t = String.split_on_char '|' i in 
                                if (List.length t) = 3 then
                                  res := (int_of_string (List.nth t 0), int_of_string (List.nth t 1), List.nth t 2) :: !res )  r;
  !res

let update (i:int) (j:int) (grid:grid) (infos_grid:infos_grid) = (* Question 9 *)
  let c = infos_grid.(i).(j) in
  Dom.Events.set_ondblclick c.container (fun _ -> Dom.Class.add c.inp "editing-input"; Dom.Focus.focus c.inp);
  Dom.Events.set_onblur c.inp (fun _ -> Dom.Text.set_content c.txt (Dom.Input.get_value c.inp); 
                                        Dom.Class.remove c.inp "editing-input"; 
                                        Storage.set (grid_to_string grid infos_grid);
                                        match Ast.make (Dom.Input.get_value infos_grid.(i).(j).inp) with 
                                          |Ok expr -> grid.(i).(j) <- expr;
                                                      c.result <- Tableur.eval_expr grid (grid.(i).(j)); 
                                                      update_display infos_grid i j c.result;
                                          |_ ->() (*Dom.Class.add infos_grid.(i).(j).container "cell-error"*));
  Dom.Events.set_onkeydown c.inp (fun a  -> if a=13 then
                                              Dom.Focus.blur c.inp;
                                              true)
let add_cell_events i j (grid:grid) (infos_grid:infos_grid) = (* Question 3 & 6 *)
  update i j grid infos_grid;
  match Ast.make (Dom.Input.get_value infos_grid.(i).(j).inp) with 
  |Ok expr -> grid.(i).(j) <- expr;
              infos_grid.(i).(j).result <- Tableur.eval_expr grid (grid.(i).(j)); 
              update_display infos_grid i j infos_grid.(i).(j).result;
  |_ -> ()

let build_cell cells = (* Question 1 & 2*)
  let c = mk_cell () in
  Dom.appendChild c.container c.inp;
  Dom.appendChild c.container c.txt;
  Dom.Class.add c.container "cell-container";
  Dom.appendChild cells c.container;
  c

let load_grids height width =
  let cells = Dom.get_element_by_id "cells" in
  Init.set_grid_template cells height width ;
  let lines = 0 -- (height - 1) in
  let columns = 0 -- (width - 1) in
  Init.build_headers cells lines columns ;
  let grid = Array.make_matrix height width Tableur.Vide in
  let infos_grid =
    Array.init height @@ fun _ -> Array.init width @@ fun _ -> build_cell cells
  in
  (grid, infos_grid)

let load_storage grid infos_grid = 
  let r = Storage.find () in
  match r with
    |None -> ()
    |Some s -> let t  = cells_of_string s in
                  List.iter (fun i -> let (a,b,c) = i in 
                                      Dom.Input.set_value infos_grid.(a).(b).inp c;
                                      Dom.Text.set_content infos_grid.(a).(b).txt c; update a b grid infos_grid) t

let main () =
  let height = 10 in
  let width = 10 in
  let (grid, infos_grid) = load_grids height width in
  let () = load_storage grid infos_grid in
  Array.iteri
    (fun i a -> Array.iteri (fun j c -> add_cell_events i j grid infos_grid) a)
    grid

let () = Init.onload main
