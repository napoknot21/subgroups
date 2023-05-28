(*This file compute the number of subgroups of Z/nZ x Z/nZ*)

(*takes an interger and calculate the number of subgroups of
   Z/nZ c Z/nZ
   -1 means infinity*)
val number_of_subgroups : int -> int

(*takes a list of couple of integers like (p,n) where p is prime and unique.
   Calculates the number of subgroups of
   Z/p^n c Z/p^n*)
val number_of_subgroups_list : (int * int) list -> int

(*Refers to the psi function in the document.
   Takes a couple (p,n) and calculate the number of subgroups of
   Z/p^nZ c Z/p^nZ*)
val number_of_subgroups_pn : int * int -> int
