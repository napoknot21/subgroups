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
      let x = f x mod n in
      let d = gcd (abs (y - x)) n in
      if i = k then loop x x (2 * k) (i + 1) d else loop x y k (i + 1) d
  in
  let x = 2 + Random.int (n - 2) in
  loop x x 2 1 1

let compute_ut n =
  let rec loop n t =
    if (n asr 1) land 1 = 1 then (n, t) else loop (n asr 1) (t + 1)
  in
  let u, t = loop (n - 1) 1 in
  ((u asr 1) lor 1, t)

let binpow a b n =
  let rec loop a res = function
    | 0 -> res
    | b ->
        let res = if b land 1 = 1 then res * a mod n else res in
        loop (a * a mod n) res (b asr 1)
  in
  loop (a mod n) 1 b

let mira_witness a n =
  Printf.printf "a = %d n = %d \n" a n;
  let rec loop x = function
    | 0 -> x <> 1
    | t ->
        let y = binpow x 2 n in
        if y = 1 then false else loop y (t - 1)
  in
  if n < 3 then false
  else if n mod 2 = 0 then true
  else
    let u, t = compute_ut n in
    Printf.printf "%d,%d \n" u t;
    loop (binpow a u n) t

let is_prime n =
  let rec loop = function
    | 0 -> true
    | t ->
        if mira_witness (1 + Random.int (n - 1)) n then false else loop (t - 1)
  in
  loop threshold

let fold_prime l =
    let rec loop res l = match res,l with
    | [], [] -> []
    | _::_, [] -> res
    | [], x::l -> loop [x] l
    | a::r as r', b::l -> if a = b then loop ((a*a)::r) l else b::r'
  in List.sort compare (loop [] l)

let rec factorise n =
  Printf.printf "Pollard Rho for %d \n" n;
  flush stdout;
  let rec loop l = function
    | 0 -> []
    | 1 -> l
    | n ->
        if n mod 2 = 0 then loop (2 :: l) (n / 2)
        else
          let d = pollard_rho pol n in
          loop (List.rev_append (factorise d) l) (n / d)
  in
  if is_prime n then [ n ] else List.sort compare (loop [] n)
