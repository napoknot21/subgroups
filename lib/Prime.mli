(* Generate the prime factorisation of an integer n with
   pollard Rho and check if a number is prime with the
   Miller-Rabin algorithm *)

(*Generates the prime factorisation of an integer in an ordered list.
  The same factors are multiplied together to obtain
  only relatively prime numbers.*)
val factorise : int -> int list

(* Check if the given argument is a prime number *)
val is_prime : int -> bool

(* Give the gcd between the two given arguments*)
val gcd : int -> int -> int
val extended_gcd : int -> int -> int * int * int
val bezout : int -> int -> int * int

(*Give the modular exponentiation as (a**b) % c*)
val binpow : int -> int -> int -> int
val fold_prime : int list -> int list
val fold_prime_couple : int list -> (int * int) list
