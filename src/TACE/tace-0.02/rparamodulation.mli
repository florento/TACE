(** Right paramodulation *)

(** Right paramodulation is only implemented for
    regular clauses. *)

open Chrono
open Trs
open Clause


(** monitor of the module of right paramodulation *)
type monitor = {
  mutable rpa_time : chrono;
  (** chronometer of the total time spent in right paramodulation. *)
}


(** global monitor, uptated by the module *)
val monitor : monitor


(** [rparamodulation r c]
    @return the list of clauses 
    obtained by right-paramodulation into the head of the given 
    clause [c] with the equations of [r].

    The clause [c] must be regular (of {! Clausetype.REG}).

    The {! Clause.clause} obtained can be of clausetype
    {! Clausetype.REG} or {! Clausetype.DEEP}.
   The subterms introduced by instanciation are marked as {i blocked} 
    in the clauses returned. *)
val rparamodulation:
  trs -> clause -> clause list 

