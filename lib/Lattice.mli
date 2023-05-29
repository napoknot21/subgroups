open Groups

type graph = int list array
type lattice = { set : group array; links : graph }
type compable = Comparable of int | Non_comparable
type table = compable array array

val make_lattice: int -> group list -> lattice
val make_relations_table: int -> group array -> table

<<<<<<< HEAD
val to_dot : int -> lattice -> out_channel -> unit
=======
val to_dot : lattice -> out_channel -> unit
>>>>>>> tmp

val compare_groups: int -> group -> group -> compable

val neighbours: int -> group array ->table -> int -> int list
