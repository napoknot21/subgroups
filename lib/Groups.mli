open Matrix

type group = { generator : int * int; mat : matrix }
type relation = Subgroups of group * group | Noncomparable of group * group

val make_relation : group -> group -> relation
