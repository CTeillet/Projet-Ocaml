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

let grid_to_string grid infos_grid = assert false (* TODO *)

let cells_of_string storage_grid = assert false (* TODO *)

let update i j grid infos_grid = assert false (* TODO *)

let add_cell_events i j grid infos_grid = assert false (* TODO *)

let build_cell cells = mk_cell () (* TODO Question 1 *)

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
