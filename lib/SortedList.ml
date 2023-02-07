(**Sorted List*)

type 'a t = { content : 'a list; cmp : 'a -> 'a -> int }

let length sl = List.length sl.content
let hd sl = List.hd sl.content
let tl sl = { content = List.tl sl.content; cmp = sl.cmp }
let nth sl = List.nth sl.content
let nth_opt sl = List.nth_opt sl.content
let iter f sl = List.iter f sl.content
let iteri f sl = List.iteri f sl.content
let filter f sl = { content = List.filter f sl.content; cmp = sl.cmp }
let make f l = { content = List.fast_sort f l; cmp = f }

let mapi f sl =
  let l = List.fast_sort sl.cmp (List.mapi f sl.content) in
  { content = l; cmp = sl.cmp }

let map f sl =
  let l = List.fast_sort sl.cmp (List.rev (List.rev_map f sl.content)) in
  { content = l; cmp = sl.cmp }

let insert x sl =
  let rec aux f x l acc =
    match l with
    | [] -> List.rev (x :: acc)
    | y :: l' ->
        if f x y = 0 then List.rev_append acc (y :: x :: l')
        else if f x y < 0 then List.rev_append acc (x :: y :: l')
        else aux f x l' (y :: acc)
  in
  { content = aux sl.cmp x sl.content []; cmp = sl.cmp }

let append v sl = make sl.cmp (List.rev_append v sl.content)
let exists e sl = List.exists (function x -> sl.cmp e x = 0) sl.content

let to_string f sl =
  let rec aux l s result =
    match s with [] -> result | x :: s' -> aux l s' (result ^ "; " ^ l x)
  in
  match sl.content with [] -> "[]" | x :: l -> "[" ^ aux f l (f x) ^ "]"

let remove x sl =
  let rec aux cmp l acc x =
    match l with
    | [] -> Some (List.rev acc)
    | e :: l' ->
        if cmp e x > 0 then None
        else if cmp e x = 0 then Some (List.rev_append acc l')
        else aux cmp l' (e :: acc) x
  in
  let l = aux sl.cmp sl.content [] x in
  match l with None -> sl | Some l -> { content = l; cmp = sl.cmp }

let compare a b =
  let rec aux cmp a b =
    match (a, b) with
    | [], [] -> 0
    | [], _ :: _ -> 1
    | _ :: _, [] -> -1
    | x :: a, y :: b ->
        let c = cmp x y in
        if c <> 0 then c else aux cmp a b
  in
  aux a.cmp a.content b.content

let pop_option sl =
  match sl.content with [] -> (None, sl) | x :: l' -> (Some x, make sl.cmp l')
