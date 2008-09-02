(** Term and atom unification procedures 
    (in empty equational theory) *)

open Term;;
open Atom;;
open Substitution;;


(** [unify s t]
    @return [Some] of the mgu (as substitution)
    of the given terms [s] and [t], or [None]
    if [s] and [t] are not unifiable. 

    Based on Robinson unification algorithm,
    hence exponential in worse case. *)
val unify:
  term -> term -> substitution option
