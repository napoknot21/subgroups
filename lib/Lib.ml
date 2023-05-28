
let binpow_mod a b n =
  let rec loop a res = function
    | 0 -> res
    | b ->
        let res = if b land 1 = 1 then res * a mod n else res in
        loop (a * a mod n) res (b asr 1)
  in
  loop a 1 b

let binpow a b =
    let rec loop a res = function
      | 0 -> res
      | b ->
          let res = if b land 1 = 1 then res * a else res in
          loop (a * a) res (b asr 1)
    in
    loop a 1 b

let sum s e f =
  let rec loop i f res =
    if i > e then res
    else loop (i+1) f (res + (f i))
  in loop s f 0
