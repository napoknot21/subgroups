(* Fucntion for matrix calculation *)

type matrix = int array array

exception Invalid_dimension

let make f n m = Array.init n (fun i -> Array.init m (fun j -> f (i, j)))
let id d = make (fun (i, j) -> if i = j then 1 else 0) d d
let size m = (Array.length m, Array.length m.(0))
let get m (i, j) = m.(i).(j)
let set m (i, j) v = m.(i).(j) <- v

let copy mat =
  let n, m = size mat in
  make (fun (i, j) -> get mat (i, j)) n m

let sum a b =
  let n, m = size a in
  if (n, m) <> size b then raise Invalid_dimension
  else make (fun (i, j) -> get a (i, j) + get b (i, j)) n m

let mul a b =
  let n, m = size a and k, l = size b in
  if m <> k then raise Invalid_dimension
  else
    let rec loop k acc (i, j) =
      if k > m then acc
      else loop (k + 1) (acc + (get a (i, k) * get b (k, j))) (i, j)
    in
    make (loop 0 0) n l

let mul_scalar mat k =
  let n, m = size mat in
  make (fun (i, j) -> k * get mat (i, j)) n m

let assign_col mat cj k cj' =
  let assign (i, j) =
    if j = cj then mat.(i).(cj) + (mat.(i).(cj') * k) else mat.(i).(j)
  in
  let n, m = size mat in
  make assign n m

let change_sign (mat, t) j =
  let change_sign_col mat cj =
    let n, m = size mat in
    make (fun (i, j) -> if cj = j then -mat.(i).(j) else mat.(i).(j)) n m
  in
  (change_sign_col mat j, change_sign_col t j)

let reduce (mat, t) li cj ck =
  let mat, t =
    if mat.(li).(cj) < 0 then change_sign (mat, t) cj else (mat, t)
  in
  let mat, t =
    if mat.(li).(ck) < 0 then change_sign (mat, t) ck else (mat, t)
  in
  let k = mat.(li).(cj) / mat.(li).(ck) in
  (assign_col mat cj (-k) ck, assign_col t cj (-k) ck)

let is_reduced (mat, _) i =
  let _, m = size mat in
  let rec loop j =
    if j >= m then true else if mat.(i).(j) <> 0 then false else loop (j + 1)
  in
  loop (i + 1)

let reduce_left (mat, t) i =
  let rec loop mt j = if j >= i then mt else loop (reduce mt i j i) (j + 1) in
  loop (mat, t) 0

let permut mat cj ck =
  let f (i, j) =
    Printf.printf "f(%d,%d)\n" i j;
    if j == cj then mat.(i).(ck)
    else if j == ck then mat.(i).(cj)
    else mat.(i).(j)
  in
  let n, m = size mat in
  make f n m

let permut_min (mat, t) i =
  let _, m = size mat in
  let rec loop index j =
    if j >= m then index
    else if mat.(i).(j) < mat.(i).(index) && mat.(i).(j) > 0 then loop j (j + 1)
    else loop index (j + 1)
  in
  let min = loop i (i + 1) in
  print_int min;
  (permut mat i min, permut t i min)

let rec hermite_loop (mat, t) i =
  let n, m = size mat in
  if i >= n || i >= m then (mat, t)
  else
    let rec loop mt j =
      print_string "hermite loop\n";
      if j >= m then
        if is_reduced mt i then reduce_left mt i else loop (permut_min mt i) i
      else if i == j then
        if mat.(i).(j) < 0 then loop (change_sign mt i) (j + 1)
        else if mat.(i).(j) = 0 then loop (permut_min mt i) i
        else loop mt (j + 1)
      else loop (reduce mt i j i) (j + 1)
    in
    hermite_loop (loop (mat, t) 0) (i + 1)

let hermite mat =
  let _, m = size mat in
  let mat,t = hermite_loop (mat, id m) 0 in
  (*let rec loop mat,t i j =
    loop (reduce_left (mat,t))*)
    mat,t


let transpose mat =
  let n, m = size mat in
  make (fun (i, j) -> mat.(j).(i)) m n

let hermite_line mat =
  let h, u = hermite (transpose mat) in
  (transpose h, transpose u)
