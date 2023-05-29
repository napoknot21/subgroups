(* Function for matrix calculation *)

type matrix = int array array

exception Invalid_dimension

let make f n m = Array.init n (fun i -> Array.init m (fun j -> f (i, j)))
let id d = make (fun (i, j) -> if i = j then 1 else 0) d d
let size m = (Array.length m, Array.length m.(0))
let get m (i, j) = m.(i).(j)
let set m (i, j) v = m.(i).(j) <- v

let makesqr a b c d =
  let f = function
  |0,0 -> a
  |1,0 -> b
  |0,1 -> c
  |1,1 -> d
  |_,_ -> failwith "makesqr"
in make f 2 2

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
  let mat = assign_col mat cj (-k) ck in
  let t = assign_col t cj (-k) ck in
  (* Check and correct the sign of pivot *)
  let mat =
    if mat.(li).(cj) < 0 then
      let adjust (i, j) = if j = cj then -mat.(i).(j) else mat.(i).(j) in
      let n, m = size mat in
      make adjust n m
    else mat
  in
  (mat, t)

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
    if j == cj then mat.(i).(ck)
    else if j == ck then mat.(i).(cj)
    else mat.(i).(j)
  in
  let n, m = size mat in
  make f n m

let min_index mat i =
  let _, m = size mat in
  let rec loop index j =
    if j >= m then index
    else if mat.(i).(j) < mat.(i).(index) && mat.(i).(j) > 0 then loop j (j + 1)
    else loop index (j + 1)
  in
  loop i (i + 1)

let permut_min (mat, t) i =
  let min = min_index mat i in
  (permut mat i min, permut t i min)

let resolve_null (mat, t) i =
  let min = min_index mat i in
  if mat.(i).(min) == 0 then None else Some (permut mat i min, permut t i min)

let rec hermite_loop_line (mat, t) i j m =
      if j >= m then
        if is_reduced (mat,t) i then reduce_left (mat,t) i
        else hermite_loop_line (permut_min (mat,t) i) i i m
      else if i == j then
        if mat.(i).(j) < 0 then hermite_loop_line (change_sign (mat,t) i) i (j + 1) m
        else hermite_loop_line (mat,t) i (j + 1) m
      else hermite_loop_line (reduce (mat,t) i j i) i (j + 1) m

let rec hermite_loop (mat, t) i =
  let n, m = size mat in
  if i >= n || i >= m then (mat, t)
  else match resolve_null (mat, t) i with
    | None -> hermite_loop (mat,t) (i+1) (* Nul line*)
    | Some mt -> hermite_loop (hermite_loop_line mt i i m) (i + 1)

let hermite mat =
  let _, m = size mat in
  let mat, t = hermite_loop (mat, id m) 0 in
  let rec make_pivot_positive mat t i =
    let n, _ = size mat in
    if i >= n then (mat, t)
    else
      let pivot = mat.(i).(i) in
      if pivot < 0 then
        let mat = mul_scalar mat (-1) in
        let t = mul_scalar t (-1) in
        make_pivot_positive mat t (i + 1)
      else
        make_pivot_positive mat t (i + 1)
  in
  let mat, t = make_pivot_positive mat t 0 in
  (mat, t)

let transpose mat =
  let n, m = size mat in
  make (fun (i, j) -> mat.(j).(i)) m n

let hermite_line mat =
  let h, u = hermite (transpose mat) in
  (transpose h, transpose u)

let union a b =
  let a_n, a_m = size a and b_n, b_m = size b in
  let n = Int.max a_n b_n and m = a_m + b_m in
  let build (i, j) =
    if i < a_n && j < a_m then a.(i).(j)
    else if i < b_n && j < m && j >= a_m then b.(i).(j - a_m)
    else 0
  in
  make build n m

let generate_hermite size deg =
  let a = make (fun _ -> Random.int deg) size size in
  let h, _ = hermite a in
  h

let is_line_null mat i =
  let _, m = size mat in
  let rec loop j =
    if j >= m then true else if mat.(i).(j) <> 0 then false else loop (j + 1)
  in
  loop 0

let isolate_not_null mat =
  let mat = transpose mat in
  let n, _ = size mat in
  let rec loop l i =
    if i >= n then l
    else if is_line_null mat i then loop l (i + 1)
    else loop (mat.(i) :: l) (i + 1)
  in
  loop [] 0

let cmp_vect a b =
  let sa = Array.length a and sb = Array.length b in
  if sa <> sb then -1
  else
    let rec loop i =
      if i >= sa then 0
      else if a.(i) > b.(i) then -1
      else if b.(i) > a.(i) then 1
      else loop (i + 1)
    in
    loop 0

let equals_not_null a b =
  let la = List.sort cmp_vect (isolate_not_null a)
  and lb = List.sort cmp_vect (isolate_not_null b) in
  List.compare cmp_vect la lb = 0
