open Subgroups.Prime
open Subgroups.Formula

let () =
  Printf.printf "\n[";
  List.iter (fun x -> Printf.printf "%d;" x) (fold_prime (factorise 0));
  Printf.printf "]\n";
  Printf.printf "\n[";
  List.iter
    (fun (p, n) -> Printf.printf "(%d,%d);" p n)
    (fold_prime_couple (factorise 0));
  Printf.printf "]\n";
  Printf.printf "|Z/2Z x Z/2Z| = %d\n" (number_of_subgroups 2);
  Printf.printf "|Z/20Z x Z/20Z| = %d\n" (number_of_subgroups 20);
  Printf.printf "|Z/0Z x Z/0Z| = %d\n" (number_of_subgroups 0);
  Printf.printf "|Z/Z x Z/Z| = %d\n" (number_of_subgroups 1);
