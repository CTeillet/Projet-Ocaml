open Tableur

let test1 () =
  let grille = cree_grille 10 10 in
  ignore grille

let test2 () =
  let grille = cree_grille 10 10 in
  grille.(0).(0) <- Entier 1 ;
  assert (grille.(0).(0) = Entier 1)

let run_tests () =
  let liste_tests =
    [("création grille", test1); ("affectation grille", test2) (* ... *)]
  in
  List.iteri
    (fun i (nom_test, f_test) ->
      Format.printf "Test #%d - %s:\t" (i + 1) nom_test ;
      try
        f_test () ;
        Format.printf "\027[32mOk\n\027[39m"
      with exn ->
        Format.printf "\027[31mErreur - %s\n\027[39m" (Printexc.to_string exn))
    liste_tests

(* Main *)
let _  = run_tests ()
