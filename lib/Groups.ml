open Matrix

type group = { generator : int * int; mat : matrix }
type relation = Subgroups of group * group | Noncomparable of group * group

let make_relation g h =
  let gm, _ = hermite g.mat and hm, _ = hermite h.mat in
  let u = union gm hm in
  if equals_not_null gm u then Subgroups (g, h)
  else if equals_not_null hm u then Subgroups (h, g)
  else Noncomparable (g, h)
