module Implem = struct end

open Parser
open Js_utils

let ( -- ) i j = List.init (j - i + 1) (fun x -> x + i)

type cell_infos = {
  container : Dom.div;
  inp : Dom.input;
  txt : Dom.txt;
  mutable result : Tableur.resultat;
}

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
    ?(txt = Dom.Create.txt " ") ?(result = Tableur.RVide) () =
  {inp; container; txt; result}

let error_to_string e = assert false (* TODO *)

let resultat_to_string r = assert false (* TODO *)

let update_display grid i j r = assert false (* TODO *)

let update_deps infos_grid i j expr = assert false (* TODO *)

let propagate grid infos_grid i j = assert false (* TODO *)

let grid_to_string grid infos_grid = (* Question 4 *)
  let res = ref [] in
  Array.iteri (fun i e -> Array.iteri (fun j f -> if grid.(i).(j) != Tableur.Vide then res := (Printf.sprintf "%d|%d|%s" i j  (Dom.Input.get_value f.inp))::!res) e) infos_grid;
  !res

let cells_of_string storage_grid = assert false (* TODO *)

let update i j grid infos_grid = assert false (* TODO *)

let add_cell_events i j (grid:grid) (infos_grid:infos_grid) = (* Question 3 *)
  let c = infos_grid.(i).(j) in
  let g = grid.(i).(j) in
  Dom.Events.set_ondblclick c.container (fun _ -> Dom.Class.add c.inp "editing-input"; Dom.Focus.focus c.inp);
  Dom.Events.set_onblur c.inp (fun _ -> Dom.Text.set_content c.txt (Dom.Input.get_value c.inp); Dom.Class.remove c.inp "editing-input");
  Dom.Events.set_onkeydown c.inp (fun a  -> if a=13 then
                                                        Dom.Focus.blur c.inp;
                                                        true)

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

let load_storage grid infos_grid = ()

let main () =
  let height = 10 in
  let width = 10 in
  let (grid, infos_grid) = load_grids height width in
  let () = load_storage grid infos_grid in
  Array.iteri
    (fun i a -> Array.iteri (fun j c -> add_cell_events i j grid infos_grid) a)
    grid

let () = Init.onload main
