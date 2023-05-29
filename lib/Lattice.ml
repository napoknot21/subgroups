open Groups
open Matrix

type graph = int list array
type lattice = { set : group array; links : graph }
type compable = Comparable of int | Non_comparable
type table = compable array array

let make_table f n m = Array.init n (fun i -> Array.init m (fun j -> f (i, j)))

let compare_groups g h =
  let ordg = ord g and ordh = ord h in
  if ordg = ordh then Non_comparable
  else
    let gm, _ = hermite g.mat and hm, _ = hermite h.mat in
    let u = union gm hm in
    if ordg > ordh && equals_not_null gm u then Comparable (-1)
    else if ordg < ordh && equals_not_null hm u then Comparable 1
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
    if i > n then false
    else if List.exists (predicate i) adj.(i) then true
    else loop (i + 1) n
  in
  loop 0 (Array.length adj)

let neighbours groups t u =
  let rec loop a i set n =
    if i >= n then set
    else
      match a.(i) with
      | Non_comparable -> loop a (i + 1) set n
      | Comparable k ->
          if k = -1 then loop a (i + 1) (i :: set) n else loop a (i + 1) set n
  in
  let res = loop t.(u) 0 [] (Array.length t.(u)) in
  List.sort (fun i j -> compare_ord groups.(i) groups.(j)) res

let rec loop_neighbours groups g u = function
  | [] -> g
  | v :: l' ->
      if links_exist groups g u v then loop_neighbours groups g u l'
      else (
        Array.set g u (v :: g.(u));
        loop_neighbours groups g u l')

let make_graph groups t =
  let rec loop_u n g u =
    if u >= n then g
    else loop_u n (loop_neighbours groups g u (neighbours groups t u)) (u + 1)
  in
  loop_u (Array.length groups)
    (Array.init (Array.length groups) (fun _ -> []))
    0

let make_lattice set_list =
  let set = Array.of_list (List.sort compare_ord set_list) in
  let t = make_relations_table set in
  {set; links = (make_graph set t)}


(* TODO *)
let to_dot lat out =
  let _,_ = lat,out  in ()

