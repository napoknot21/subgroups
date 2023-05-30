(* Include necessary modules *)
open Subgroups.Formula
open Subgroups.Prime
open Subgroups.Groups
open Subgroups.Lattice

(* Command line arguments *)
let n = ref 0
let p = ref false
let g = ref true
let o = ref "output.dot"

(* Specification of command line options *)
let specs = [
  ("-n", Arg.Int (fun x -> n := x), "Set the value of n");
  ("-p", Arg.Set p, "Decomposition into prime numbers");
  ("-g", Arg.Set g, "Whether to draw a graph (default is true)");
  ("-o", Arg.String (fun s -> o := s), "Set the output file name")
]


let () =
  Arg.parse specs print_endline "Usage: main.ml -n <n_value> [-p] [-g] [-o output_file_name]";

  let n_value = !n in
  if n_value < 1 then (
    Printf.printf "Please provide a valid value for n (n > 0).\n";
    exit 1
  );

  let prime_decomposition = !p in
  let draw_graph = !g in
  let output_file_name = !o in

  Printf.printf "|Z/%dZ x Z/%dZ| = %d\n" n_value n_value (number_of_subgroups n_value);
  Printf.printf "Generating subgroups...";
  flush stdout;

  let set = generate_subgroups 2 3 in
  Printf.printf "done\nGenerating lattice...";
  flush stdout;
  
  let lattice = make_lattice set in
  Printf.printf "done\n";
  
  if draw_graph then (
    Printf.printf "Writing the graph dot file...";
    flush stdout;
    let file = open_out output_file_name in
    to_dot lattice file;
    close_out file;
    Printf.printf "done\n"
  );
  
  if prime_decomposition then (
    let factors = Prime.factorise n_value in
    List.iter (fun (p, m) -> Printf.printf "(%d^%d) " p m) factors;
    Printf.printf "\n"
  )


(*
  Arg.parse speclist (fun _ -> ()) "Usage: program [-n n] [-g] [-o output] [-p]";

  let n = !n in
  let draw_graph = !draw_graph in
  let output = !output in
  let print_prime = !print_prime in

  if n = 0 then failwith "Please set a non-zero n using -n";

  if print_prime then (
    let primes = Prime.factorise n in
    Printf.printf "Prime factorisation: %s\n" (string_of_int_list primes);
  );

  Printf.printf "|Z/%dZ x Z/%dZ| = %d\n" n n (number_of_subgroups n);
  Printf.printf "Generating subgroups...";
  flush stdout;

  if Prime.fold_prime_couple (Prime.factorise n) > 1 then (
    Printf.printf "Sorry, the program can only generate graphs for p^m.\n";
    exit 1
  );

  let set = generate_subgroups 2 3 in

  Printf.printf "done\nGenerating lattice...";
  flush stdout;
  
  let lattice = make_lattice set in

  Printf.printf "done\n";

  if draw_graph then (
    Printf.printf "Writing the graph dot file...";
    flush stdout;
    let file = open_out output in
    to_dot lattice file;
    close_out file;
    Printf.printf "done\n"
  )
*)
(*
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

  *)