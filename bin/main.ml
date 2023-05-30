open Subgroups.Prime
open Subgroups.Formula
open Subgroups.Groups
open Subgroups.Lattice

let test =
  let set = generate_subgroups 2 1 in
  let lattice = make_lattice set in
  ()
let n = 8

let () =
  Printf.printf "|Z/%dZ x Z/%dZ| = %d\n" n n (number_of_subgroups n);
  Printf.printf "Generating subgroups...";
  flush stdout;
  let set = generate_subgroups 2 3 in
  Printf.printf "done\nGenerating lattice...";
  flush stdout;
  let lattice = make_lattice set in
  Printf.printf "done\nWriting the graph dot file...";
  flush stdout;
  let file = open_out "output.dot" in
    to_dot lattice file;
  close_out file;
  Printf.printf "done\n"
  (* let file = open_out "output.txt" in
    for i = 0 to 5000 do
      Printf.fprintf file "%d %d\n" i (number_of_subgroups i);
    done;
    close_out file; *)


  (* Printf.printf "\n[";
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
  Printf.printf "|Z/Z x Z/Z| = %d\n" (number_of_subgroups 1); *)
