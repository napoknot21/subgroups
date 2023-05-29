open Matrix

type group = { generator : int * int; mat : matrix }

val ord: group -> int

val compare_ord: group -> group -> int
