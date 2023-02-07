(**Stack*)

type 'a t
(** Stack.t is an abstract type, see Fifo.ml for the concrete implementation *)

val empty : 'a t
(** Empty Stack *)

val push : 'a -> 'a t -> 'a t
(** Adds an element to the stack.
    It is placed first in the order of future retrievals *)

val push_front : 'a -> 'a t -> 'a t
(** Adds an element in the back of the stack *)

val push_list : 'a list -> 'a t -> 'a t
(** Adds all elements of the list in the stack
    The elements of the list are added from the left to the right*)

val pop : 'a t -> 'a * 'a t
(** Retrieves and removes the last element in the stack,
    or raises Not_found if empty *)

val pop_option : 'a t -> 'a option * 'a t
(** Retrieves and removes some last element in the stack,
	or none if empty *)

val pop_n : 'a t -> int -> 'a list * 'a t
(** Retrieves and removes n elements for the end of the stack.
    Removes the whole stack content if n is greater than the size of the stack.
    Raises Not_found if the stack empty *)

val of_list : 'a list -> 'a t
(** Adds all elements of a list in an empty stack.
    The head of the list is added first. *)

val to_list : 'a t -> 'a list
(** Retrieves all the elements of a satck.
    The fist element added in the stack is the head of the output list. *)

val substack : 'a t -> int -> 'a t * 'a t
(** Extracts n elements of the stack conserving their retrivals position*)

val to_string : ('a -> string) -> 'a t -> string
(** Render the stack in a string containing its contents tranformed by
    the function*)

val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int

val length : 'a t -> int
