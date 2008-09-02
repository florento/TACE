 (** Detection of some tautologies *)

(** In this implementation, we define a orn clause to 
    be a tautology when
    its head is equal to a negative literal. *)

open Chrono
open Clause


(** monitor of the module *)
type monitor = {
  mutable tau_tested : int;
  (** total number of resolution steps (successful or not) *)
  mutable tau_success : int;
  (** number of successful resolution steps. *)
  mutable tau_time : chrono;
  (** chronometer of the total time spent in tautologies testing. *)
}


(** global monitor, uptated by the module *)
val monitor : monitor


(** [tautology c]
    @return true iff the tclause [c] is a tautology, 
    according to the above definition. 
    
    [c] must be a Horn clause.

    A clause of {! Clausetype.clausetype} [UNDEF] 
    is assumed to be a tautology. *)
val tautology:
  clause -> bool


