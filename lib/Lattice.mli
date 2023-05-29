open Groups

type graph = int list array
type lattice = { set : group array; links : graph }
type compable = Comparable of int | Non_comparable
type table = compable array array

val make_lattice : group list -> lattice
val make_relations_table : group array -> table
val to_dot : lattice -> out_channel -> unit
val compare_groups : group -> group -> compable
val neighbours : group array -> table -> int -> int list
