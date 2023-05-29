open Matrix

type group = { card : int; mat : matrix }

val compare_ord : group -> group -> int
val generate_subgroups : int -> int -> group list
