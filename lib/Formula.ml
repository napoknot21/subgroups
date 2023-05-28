(*This file compute the number of subgroups of Z/nZ x Z/nZ*)
open Lib


let number_of_subgroups_pn (p, n) =
  let f i = (n - i)* (binpow p i)
  and g i = (1 - binpow p (n - i + 1))/(1 - p)
in sum 0 n f + sum 0 n g


let number_of_subgroups_list l =
  let rec loop res = function
    | [] -> res
    | e :: l' -> loop (res * number_of_subgroups_pn e) l'
  in
  loop 1 l

let number_of_subgroups n =
  if n = 0 then -1
  else if n = 1 then 1
  else number_of_subgroups_list (Prime.fold_prime_couple (Prime.factorise n))
