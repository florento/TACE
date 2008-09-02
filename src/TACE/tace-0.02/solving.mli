(** Equation solving *)

(** by unification (one step)
    or several steps of left-paramodulation and unification. *)

open Chrono
open Trs
open Clause


(** monitor of the module of right paramodulation *)
type monitor = {
  mutable sol_time : chrono;
  (** chronometer of the total time spent in left paramodulation.
      CAUTION: the time recorder by the chrono embeds the time
      recorded by {! Lparamodulation.monitor.lpa_time}. *)
}


(** global monitor, uptated by the module *)
val monitor : monitor


(** [solve c]
    @return the list of clauses obtained by 
    solving, by unification, 
    one of the negative selected equations of [c].

    The subterms introduced by instanciation are marked as {i blocked} 
    in the clauses returned. *)
val solve:
  clause -> clause list 


(** [solve_all r c]
    @return the list of clauses obtained by solving, with [r]
    and saturation of left-paramodulation and solving by unification, 
    all the negative equations in the given clause [c].

    The subterms introduced by instanciation are marked as {i blocked} 
    in the clauses returned. *)
val solve_all:
  trs -> clause -> clause list 

