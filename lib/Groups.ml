open Matrix
open Lib


type group = { generator : int * int; mat : matrix }


let ord g = g.mat.(0).(0) * g.mat.(1).(1)

let compare_ord g h =
  Int.compare (ord g) (ord h)


let generate_a_0 n =
  let rec loop_j set i j =
    if j > n - i then set
    else loop_j ((i,j) :: set) i (j+1)
  in
  let rec loop_i set i =
    if i > n then set
    else loop_i (loop_j set i 0) (i+1)
  in loop_i [] 0


let generate_a_k n k =
  if k = 0 then generate_a_0 n
  else
    let rec loop_i set i =
      if i > n then set
      else loop_i ((i, n + k - i)::set) (i+1)
    in loop_i [] k


let generate_m_k p n k =
  let a_k = generate_a_k n k in
  let rec loop_i set pa pb m i =
    if i >= m then set
    else loop_i ((makesqr pa i 0 pb)::set) pa pb m (i+1)
  in
  let rec loop set = function
  | [] -> set
  | (a,b)::l' ->
    loop (loop_i set (binpow p a) (binpow p b) (binpow p (b-k)) 0) l'
in loop [] a_k

let matrix_to_groups pn m =
let a,b = m.(0).(0) mod pn, m.(1).(0) mod pn
and d = m.(1).(1) mod pn in
(a,b),(0,d)


let generate_subgroups p n =
  let rec loop set i =
    if i > n then set
    else loop (List.rev_append set (generate_m_k p n i)) (i+1)
  in
  List.rev_map (matrix_to_groups (binpow p n)) (loop [] 0)
