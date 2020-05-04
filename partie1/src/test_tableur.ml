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

let test14 () =
  let grille = cree_grille 10 10 in
  grille.(0).(0) <- Case (1,1);
  let f (_:resultat) = RVide in
  grille.(1).(1) <- Unaire({app1=f; operande = Case(0,0)});
  assert(cycle grille (Case(0,0)) = true)

let test15 () =
  let grille = cree_grille 10 10 in
  grille.(0).(0) <- Case (1,1);
  grille.(5).(5) <- Entier 5;
  let f (_:resultat) = RVide in
  grille.(1).(1) <- Unaire({app1=f; operande = Case(5,5)});
  assert(cycle grille (Case(0,0)) = false)

let test16 () =
  let grille = cree_grille 10 10 in
  grille.(0).(0) <- Case (1,1);
  grille.(5).(5) <- Case (1,1);
  let f (_:resultat) (_:resultat) = RVide in
  grille.(1).(1) <- Binaire({app2=f; gauche = Case(5,5); droite=Case(0,0)});
  assert(cycle grille (Case(1,1)) = true)

let test17 () =
  let grille = cree_grille 10 10 in
  grille.(0).(0) <- Case (1,8);
  grille.(5).(1) <- Case (1,1);
  let f (_:resultat) (_:resultat) = RVide in
  grille.(1).(1) <- Binaire({app2=f; gauche = Case(5,5); droite=Case(0,0)});
  assert(cycle grille (Case(1,1)) = false)

let test18 () =
  let grille = cree_grille 10 10 in
  let f (_:resultat) (_:resultat) = RVide in
  grille.(0).(1) <- Case(1,1);
  grille.(1).(1) <- Case(0,1);
  grille.(0).(3) <- Reduction({app=f; case_debut = (0,0); case_fin=(2,2); init=REntier 0});
  assert(cycle grille (Case(0,3)) = true)

let test19 () =
  let grille = cree_grille 10 10 in
  let f (_:resultat) (_:resultat) = RVide in
  grille.(0).(1) <- Entier 1;
  grille.(1).(1) <- Chaine "test";
  grille.(2).(0) <- Flottant 2.;
  grille.(2).(1) <- Case(0,1);
  grille.(2).(2) <- Case(2,0);
  grille.(0).(3) <- Reduction({app=f; case_debut = (0,0); case_fin=(2,2); init=REntier 0});
  assert(cycle grille (Case(0,3)) = false)

let test20 () =
  let grille = cree_grille 10 10 in
  grille.(0).(1) <- Entier 2;
  grille.(0).(0) <- abs (Case(0,1));
  assert(eval_expr grille (Case(0,0)) = REntier 2)
  
let test21 () =
  let grille = cree_grille 10 10 in
  grille.(0).(1) <- Entier (-2);
  grille.(0).(0) <- abs (Case(0,1));
  assert(eval_expr grille (Case(0,0)) = REntier 2)

let test22 () =
  let grille = cree_grille 10 10 in
  grille.(0).(1) <- Entier (-2);
  grille.(1).(1) <- Entier 2;
  grille.(0).(0) <- add (Case(0,1)) (Case(1,1));
  assert(eval_expr grille (Case(0,0)) = REntier 0)

let test23 () =
  let grille = cree_grille 10 10 in
  grille.(0).(1) <- Entier (-2);
  grille.(1).(1) <- Flottant 2.;
  grille.(0).(0) <- add (Case(0,1)) (Case(1,1));
  assert(eval_expr grille (Case(0,0)) = RFlottant 0.)

let test24 () =
  let grille = cree_grille 10 10 in
  grille.(0).(1) <- Entier 2;
  grille.(1).(1) <- Entier 2;
  grille.(0).(0) <- Entier 2;
  grille.(1).(0) <- Entier 2;
  grille.(3).(0) <- somme (0,0) (1,1);
  assert(eval_expr grille (Case(3,0)) = REntier 8)

let test25 () =
  let grille = cree_grille 10 10 in
  grille.(0).(1) <- Entier 2;
  grille.(1).(1) <- Flottant 2.;
  grille.(0).(0) <- Flottant 2.;
  grille.(1).(0) <- Entier 2;
  grille.(3).(0) <- somme (0,0) (1,1);
  assert(eval_expr grille (Case(3,0)) = RFlottant 8.)

let test26 () =
  let grille = cree_grille 10 10 in
  grille.(0).(1) <- Entier (-2);
  grille.(0).(0) <- oppose (Case(0,1));
  assert(eval_expr grille (Case(0,0)) = REntier 2)

let test27 () =
  let grille = cree_grille 10 10 in
  grille.(0).(1) <- Entier (10);
  grille.(0).(0) <- inverse (Case(0,1));
  assert(eval_expr grille (Case(0,0)) = REntier 0)

let test28 () =
  let grille = cree_grille 10 10 in
  grille.(0).(1) <- Flottant (10.);
  grille.(0).(0) <- inverse (Case(0,1));
  assert(eval_expr grille (Case(0,0)) = RFlottant 0.1)

let test29 () =
  let grille = cree_grille 10 10 in
  grille.(0).(1) <- Entier (-2);
  grille.(1).(1) <- Flottant 2.;
  grille.(0).(0) <- minus (Case(0,1)) (Case(1,1));
  assert(eval_expr grille (Case(0,0)) = RFlottant (-4.))

let test30 () =
  let grille = cree_grille 10 10 in
  grille.(0).(1) <- Entier (-2);
  grille.(1).(1) <- Flottant 2.;
  grille.(0).(0) <- mul (Case(0,1)) (Case(1,1));
  assert(eval_expr grille (Case(0,0)) = RFlottant (-4.))

let run_tests () =
  let liste_tests =
    [("création grille", test1); ("affectation grille", test2); ("Avec cycle simple", test3); ("Sans cycle simple", test4); ("Avec cycle plus long", test5);
    ("Sans cycle plus long", test6); ("affichage case", test7); ("affichage entier", test8); ("affichage chaine", test9); ("affichage flottant", test10);
    ("test eval avec case", test11); ("test eval avec multiple case avant arrivée", test12); ("test eval avec case en dehors de la grille", test13);
    ("test cycle vrai avec unaire", test14); ("test cycle faux avec unaire", test15) ; ("test cycle vrai avec binaire", test16); ("test cycle faux avec binaire", test17);
    ("test cycle vrai reduction", test18);  ("test cycle faux reduction", test19);  ("test eval avec unaire absolue", test20);  ("test eval avec valeur negative unaire absolue", test21);
    ("test eval avec somme", test22);  ("test eval avec somme int + float", test23); ("test eval avec reduction somme", test24); ("test eval avec reduction somme melange int float", test25);
    ("test eval avec oppose", test26); ("test eval avec inverse entier", test27); ("test eval avec inverse flottant", test28); ("test eval avec difference", test29);
    ("test eval avec multiplication bianire", test30)]
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
