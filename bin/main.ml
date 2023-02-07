open Subgroups.Prime

let binpow a b n =
  let rec loop a res = function
    | 0 -> res
    | b ->
        let res = if b land 1 = 1 then res * a mod n else res in
        loop (a * a mod n) res (b asr 1)
  in
  (loop a 1 b)

let () = (fun x -> Printf.printf "%b \n" x) (is_prime 2764553);

