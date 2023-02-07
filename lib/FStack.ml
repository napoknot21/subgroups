(**Stack*)

type 'a t = 'a list

let empty = []
let push x s = x :: s
let push_front x s = List.rev (x :: List.rev s)

let rec push_list l s =
  match l with [] -> s | x :: l' -> push_list l' (push x s)

let pop = function [] -> raise Not_found | x :: s' -> (x, s')

let pop_option = function [] -> (None, empty) | x::s' -> (Some x ,s')

let pop_n s n =
  let rec aux s n l =
    if n <= 0 then (l, s)
    else
      let x, s' = pop s in
      aux s' (n - 1) (x :: l)
  in
  aux s n []

let of_list l = push_list l empty
let to_list = function [] -> [] | s -> List.rev s

let substack s n =
  let l, s' = pop_n s n in
  (of_list l, s')

let to_string f s =
  let rec aux f s result =
    match s with [] -> result | x :: s' -> aux f s' (result ^ "; " ^ f x)
  in
  match s with [] -> "[]" | x :: s -> "[" ^ aux f s (f x) ^ "]"

let compare cmp a b=
  let rec aux cmp a b =
  match a,b with
  | [], [] -> 0
  | [], _::_ -> 1
  | _::_, [] -> -1
  |x::a, y::b -> let c = cmp x y in
  if c <> 0 then c
  else aux cmp a b
in aux cmp a b

let length = List.length
