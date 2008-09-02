(** Term and atom matching *)

open Term
open Substitution


(** [matcher t1 t2]
    @return [Some] of the matcher for the term [t1] and [t2],
    i.e. a substitution [subst] such that [apply subst t1 = t2],
    or [None] if [t1] does not match [t2]. *)
val matcher:
  term -> term -> substitution option


(** [matching t1 t2]
    @return [true] if [t1] matches [t2], [false] otherwise. *)
val matching:
  term -> term -> bool

