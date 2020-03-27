open Tableur

let test1 () =
  let grille = cree_grille 10 10 in
  ignore grille

let test2 () =
  let grille = cree_grille 10 10 in
  grille.(0).(0) <- Entier 1 ;
  assert (grille.(0).(0) = Entier 1)

let test3 () =
  let grille = cree_grille 10 10 in
  grille.(0).(0) <- Case (1,1);
  grille.(1).(1) <- Case (0,0);
  assert (cycle grille (Case (0,0)) = true)

let test4 () =
  let grille = cree_grille 10 10 in
  grille.(0).(0) <- Entier 0;
  grille.(1).(1) <- Case (0,0);
  assert (cycle grille (Case (0,0)) = false)

let test5 () = 
  let grille = cree_grille 10 10 in
  grille.(0).(0) <- Case (1,1);
  grille.(1).(1) <- Case (2,2);
  grille.(2).(2) <- Case (3,3);
  grille.(3).(3) <- Case (4,4);
  grille.(4).(4) <- Case (0,0);
  assert (cycle grille (Case (0,0)) = true)

let test6 () = 
  let grille = cree_grille 10 10 in
  grille.(0).(0) <- Case (1,1);
  grille.(1).(1) <- Case (2,2);
  grille.(2).(2) <- Case (3,3);
  grille.(3).(3) <- Case (4,4);
  grille.(4).(4) <- Entier 10;
  assert (cycle grille (Case (0,0)) = false)

let test7 () =
  assert (expr_to_string (Case(1,2)) = "@(1,2)")

let test8 () =
  assert (expr_to_string (Entier 10) = "10")

let test9 () =
  assert (expr_to_string (Chaine "Bonjour") = "Bonjour")

let test10 () =
  assert (expr_to_string (Flottant 1.5) = "1.5")

let test11 ()=
  let grille = cree_grille 10 10 in
  grille.(0).(0) <- Entier 0;
  grille.(0).(1) <- Case (0,0);
  let grille_res = eval_grille grille in
  assert(grille_res.(0).(1) = REntier 0)

let test12 () =
  let grille = cree_grille 10 10 in
  grille.(0).(0) <- Flottant 1.2;
  grille.(0).(1) <- Case (0,0);
  grille.(0).(2) <- Case (0,1);
  grille.(0).(3) <- Case (0,2);
  grille.(1).(2) <- Case (0,3);
  grille.(1).(3) <- Case (1,2);
  grille.(5).(0) <- Case (1,3);
  let grille_res = eval_grille grille in
  assert(grille_res.(5).(0) = RFlottant 1.2)

let test13 () =
  let grille = cree_grille 10 10 in
  grille.(0).(0) <- Case (10,12);
  let grille_res = eval_grille grille in
  assert(grille_res.(0).(0) = Erreur(Mauvais_indice (10,12)))


let run_tests () =
  let liste_tests =
    [("création grille", test1); ("affectation grille", test2); ("Avec cycle simple", test3); ("Sans cycle simple", test4); ("Avec cycle plus long", test5);
    ("Sans cycle plus long", test6); ("affichage case", test7); ("affichage entier", test8); ("affichage chaine", test9); ("affichage flottant", test10);
    ("test eval avec case", test11); ("test eval avec multiple case avant arrivée", test12); ("test eval avec case en dehors de la grille", test13) ]
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
