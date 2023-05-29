open Matrix

type group = { generator : int * int; mat : matrix }

val ord: int -> group -> int

val compare_ord: int -> group -> group -> int

val generate_subgroups: int -> int -> group list
