(* Generate the prime factorization of an integer n with
   pollard Rho algortihm and check if a number is prime with
   	Miller-Rabin algorithm *)
let threshold = 50
let pol x = (x * x) - 1
let rec gcd a b = if b = 0 then a else gcd b (a mod b)

let pollard_rho f n =
  let rec loop x y k i d =
    if d <> 1 then d
    else
      let x = f x mod n and d = gcd (abs (y - x)) n in
      if i = k then loop x x (2 * k) (i + 1) d else loop x y k (i + 1) d
  in
  let x = 2 + Random.int n in
  loop x x 2 1 1

let nth_bit x i = x land (1 lsl i) <> 0

let bin_to_int b =
  let rec loop res = function
    | [] -> res
    | b :: l ->
        if res = 0 then loop 1 l
        else if b then loop (res * 2 + 1) l else loop (res * 2) l
  in
  loop 0 b

let int_to_bin n =
  let rec loop l k i =
    if k == 0 then l else loop (nth_bit n i :: l) (k / 2) (i + 1)
  in
  List.rev(loop [] n 0)

(*let bin_to_int b = bin_to_int_at b 0*)

let compute_ut n =
  let rec loop t = function
    | [] -> (t, [])
    | b :: l -> if b then (t, l) else loop (t + 1) l
  in
  let b = int_to_bin (n-1) in
  let t, l = loop 0 b in
  match l with [] -> (0, t) | l -> (bin_to_int (List.rev(true :: l)), t)

let binpow a b n =
  let rec loop a res = function
    | 0 -> 0
    | b ->
        let res = if b land 1 = 1 then (res * a) mod n else res in
        loop ((a * a) mod n) res (b asr 1)
  in
  loop (a mod n) 1 b

let mira_witness a n =
  let rec loop x t =
    if t = 0 then x <> 1
    else
      let y = (x * x) mod n in
      if y = 1 then false else loop y (t - 1)
  in
  if n < 3 then false
  else if n mod 2 = 0 then true
  else
    let u, t = compute_ut n in
    Printf.printf "%d\n" (binpow a u n);
    loop (binpow a u n) t

let is_prime n =
  let rec loop t =
    if t = 0 then true
    else if mira_witness (Random.int n + 1) n then false
    else loop (t - 1)
  in
  loop threshold

let rec factorise n =
  let rec loop l = function
    | 0 -> []
    | 1 -> l
    | n ->
        if n mod 2 = 0 then loop (2 :: l) (n / 2)
        else
          let d = pollard_rho pol n in
          Printf.printf "%d; " d;
          loop (List.rev_append (factorise d) l) (n / d)
  in
  if is_prime n then [ n ] else loop [] n
