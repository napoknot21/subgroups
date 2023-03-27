(* Function for matrix calculation *)

type matrix = int array array

exception Invalid_dimension

val id : int -> matrix
val copy : matrix -> matrix
val make : (int * int -> int) -> int -> int -> matrix
val size : matrix -> int * int
val get : matrix -> int * int -> int
val set : matrix -> int * int -> int -> unit
val sum : matrix -> matrix -> matrix
val mul : matrix -> matrix -> matrix
val mul_scalar: matrix -> int -> matrix
val hermite : matrix -> matrix * matrix
val change_sign: matrix -> matrix -> int -> matrix * matrix
val reduce: matrix -> matrix -> int -> int -> int -> matrix*matrix
val nullify: matrix -> matrix -> int -> int -> int -> matrix* matrix
