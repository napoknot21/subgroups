open Groups
open Matrix

type graph = int list array
type lattice = { set : group array; links : graph }
type compable = Comparable of int | Non_comparable
type table = compable array array

let make_table f n m = Array.init n (fun i -> Array.init m (fun j -> f (i, j)))

let compare_groups g h =
  if g.card = h.card then Non_comparable
  else
    let u, _ = hermite (union g.mat h.mat) in
    if g.card > h.card && equals_not_null g.mat u then Comparable (-1)
    else if g.card < h.card && equals_not_null h.mat u then Comparable 1
    else Non_comparable

let make_relations_table set =
  let n = Array.length set and f (i, j) = compare_groups set.(i) set.(j) in
  make_table f n n

let links_exist groups adj u v =
  let predicate u' v' =
    if v <> v' then false
    else
      match compare_groups groups.(u) groups.(u') with
      | Non_comparable -> false
      | Comparable k -> k = -1
  in
  let rec loop i n =
    if i >= n then false
    else if List.exists (predicate i) adj.(i) then true
    else loop (i + 1) n
  in
  loop 0 (Array.length adj)

let neighbours t u =
  let rec loop a i set m =
    if i >= m then set
    else
      match a.(i) with
      | Non_comparable -> loop a (i + 1) set m
      | Comparable k ->
          if k = -1 then loop a (i + 1) (i :: set) m else loop a (i + 1) set m
  in
  loop t.(u) 0 [] (Array.length t.(u))

let rec loop_neighbours groups g u = function
  | [] -> g
  | v :: l' ->
      if links_exist groups g u v then loop_neighbours groups g u l'
      else (
        Array.set g u (v :: g.(u));
        loop_neighbours groups g u l')

let make_graph groups t =
  let rec loop_u m g u =
    if u >= m then g
    else loop_u m (loop_neighbours groups g u (neighbours t u)) (u + 1)
  in
  loop_u (Array.length groups)
    (Array.init (Array.length groups) (fun _ -> []))
    0

let make_lattice set_list =
  let set = Array.of_list (List.sort compare_ord set_list) in
  let t = make_relations_table set in
  { set; links = make_graph set t }

(* TODO *)
let to_dot lat out =
  let n = Array.length lat.set in
  let matrix_to_string m =
    let n, _ = Matrix.size m in
    let rows =
      Array.init n (fun i ->
          let row = m.(i) in
          String.concat "," (List.map string_of_int (Array.to_list row)))
    in
    "[" ^ String.concat "]\n[" (Array.to_list rows) ^ "]"
  in
  let print_edge u v = Printf.fprintf out "%d -> %d;\n" u v in
  Printf.fprintf out "digraph G {\n";
  for i = 0 to n - 1 do
    let g = lat.set.(i) in
    Printf.fprintf out "%d [label=\"%s\"];\n" i (matrix_to_string g.mat);
    List.iter (print_edge i) lat.links.(i)
  done;
  Printf.fprintf out "}\n"
