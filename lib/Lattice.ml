open Groups
open Matrix

type graph = int list array
type lattice = { set : group array; links : graph }
type compable = Comparable of int | Non_comparable
type table = compable array array

let make_table f n m = Array.init n (fun i -> Array.init m (fun j -> f (i, j)))

let compare_groups n g h =
  let ordg = ord n g and ordh = ord n h in
  if ordg = ordh then Non_comparable
  else
    let u,_ = hermite (union g.mat h.mat) in
    if equals_not_null g.mat u then Comparable (-1)
    else if equals_not_null h.mat u then Comparable 1
    else Non_comparable

let make_relations_table n set =
  let n = Array.length set and f (i, j) = compare_groups n set.(i) set.(j) in
  make_table f n n

let links_exist n groups adj u v =
  let predicate u' v' =
    if v <> v' then false
    else
      match compare_groups n groups.(u) groups.(u') with
      | Non_comparable -> false
      | Comparable k -> k = -1
  in
  let rec loop i n =
    if i >= n then false
    else if List.exists (predicate i) adj.(i) then true
    else loop (i + 1) n
  in
  loop 0 (Array.length adj)

let neighbours n groups t u =
  let rec loop a i set n =
    if i >= n then set
    else
      match a.(i) with
      | Non_comparable -> loop a (i + 1) set n
      | Comparable k ->
          if k = -1 then loop a (i + 1) (i :: set) n else loop a (i + 1) set n
  in
  let res = loop t.(u) 0 [] (Array.length t.(u)) in
  List.sort (fun i j -> (compare_ord n) groups.(i) groups.(j)) res

let rec loop_neighbours n groups g u = function
  | [] -> g
  | v :: l' ->
      if links_exist n groups g u v then loop_neighbours n groups g u l'
      else (
        Array.set g u (v :: g.(u));
        loop_neighbours n groups g u l')

let make_graph n groups t =
  let rec loop_u m g u =
    if u >= m then g
    else loop_u m (loop_neighbours n groups g u (neighbours n groups t u)) (u + 1)
  in
  loop_u (Array.length groups)
    (Array.init (Array.length groups) (fun _ -> []))
    0

let make_lattice n set_list =
  let set = Array.of_list (List.sort (compare_ord n) set_list) in
  let t = make_relations_table n set in
  {set; links = (make_graph n set t)}


(* TODO *)
let to_dot n lat out =
  let _ = n in
  let print_edge u v =
    Printf.fprintf out "  %d -> %d;\n" u v
  in
  let print_node i =
    Printf.fprintf out "  %d [label=\"%d\"];\n" i i
  in
  Printf.fprintf out "digraph {\n";
  Array.iteri (fun i _ -> print_node i) lat.set;
  Array.iteri (fun u vs -> List.iter (print_edge u) vs) lat.links;
  Printf.fprintf out "}\n"

