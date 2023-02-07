(**Sorted list*)

type 'a t = { content : 'a list; cmp : 'a -> 'a -> int }

val length : 'a t -> int
val hd : 'a t -> 'a
val tl : 'a t -> 'a t
val nth : 'a t -> int -> 'a
val nth_opt : 'a t -> int -> 'a option
val iter : ('a -> unit) -> 'a t -> unit
val map : ('a -> 'a) -> 'a t -> 'a t
val iteri : (int -> 'a -> unit) -> 'a t -> unit
val mapi : (int -> 'a -> 'a) -> 'a t -> 'a t
val filter : ('a -> bool) -> 'a t -> 'a t
val make : ('a -> 'a -> int) -> 'a list -> 'a t

val append : 'a list -> 'a t -> 'a t
(**Appends the second argument in the first and keep the list sorted*)

val exists : 'a -> 'a t -> bool
(**Checks if an element exist in the list*)

val insert : 'a -> 'a t -> 'a t
(**Insert an element into the sorted list*)

val to_string : ('a -> string) -> 'a t -> string
(** Render the stack in a string containing its contents tranformed by
    the function*)

val remove : 'a -> 'a t -> 'a t
val compare : 'a t -> 'a t -> int
val pop_option : 'a t -> 'a option * 'a t
