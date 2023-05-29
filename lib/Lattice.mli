open Groups

type graph = int list array
type lattice = { set : group array; links : graph }
type compable = Comparable of int | Non_comparable
type table = compable array array

val make_lattice: group list -> lattice

val to_dot : lattice -> out_channel -> unit
