open Subgroups

(* is_prime *)
let%test _ = is_prime 5 = true
let%test _ = is_prime 10 = false
let%test _ = is_prime 82525242419 = true
(* factorise *)
let%test _ = factorise 247575727257 = [ (3, 82525242419) ]
let%test _ = factorise 21 = [ (3, 7) ]
OUnit.
