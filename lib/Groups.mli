open Matrix

type group = { generator : int * int; mat : matrix }

val ord: group -> int

val compare_ord: group -> group -> int

val generate_subgroups: int -> int -> ((int * int) * (int * int)) list
