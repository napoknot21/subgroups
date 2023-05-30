(* Include necessary modules *)
open Subgroups.Formula
open Subgroups.Prime
open Subgroups.Groups
open Subgroups.Lattice

(* Command line arguments *)
let n = ref None
let p = ref false
let g = ref true
let o = ref "output.dot"

(* Specification of command line options *)
let specs =
  [
    ("-n", Arg.Int (fun x -> n := Some x), "Set the value of n");
    ("-p", Arg.Set p, "Decomposition into prime numbers");
    ("-g", Arg.Set g, "Whether to draw a graph (default is true)");
    ("-o", Arg.String (fun s -> o := s), "Set the output file name");
  ]

let () =
  Arg.parse specs print_endline
    "Usage: main.ml -n <n_value> [-p] [-g] [-o output_file_name]";

  match !n with
  | None ->
      Printf.printf "Please provide a valid value for n (n > 0).\n";
      exit 1
  | Some n_value ->
      let prime_decomposition = !p in
      let draw_graph = !g in
      let output_file_name = !o in

      if (not draw_graph) && output_file_name <> "output.dot" then (
        Printf.printf "The -o flag can't be used without the -g flag.\n";
        exit 1);
      let l = fold_prime_couple (factorise n_value) in
      if prime_decomposition then (
        Printf.printf "[";
        List.iter (fun (p, n) -> Printf.printf "(%d^%d);" p n) l;
        Printf.printf "]\n")
      else (
        Printf.printf "|Z/%dZ x Z/%dZ| = %d\n" n_value n_value
          (number_of_subgroups n_value);

        if List.length l <> 1 || !n = Some(1) then (
          Printf.printf "n must be p^m where p is prime !\n";
          exit 1)
        else (
          Printf.printf "Generating subgroups...";
          flush stdout;
          let (a,b) = List.hd l in
          let set = generate_subgroups a b in
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
            Printf.printf "done\n")))
