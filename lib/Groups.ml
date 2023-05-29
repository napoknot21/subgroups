open Matrix

type group = { generator : int * int; mat : matrix }


let ord g = g.mat.(0).(0) * g.mat.(1).(1)

let compare_ord g h =
  Int.compare (ord g) (ord h)
