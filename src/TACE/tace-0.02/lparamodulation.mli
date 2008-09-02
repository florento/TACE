(** Left paramodulation *)

open Chrono
open Trs
open Clause


(** monitor of the module of right paramodulation *)
type monitor = {
  mutable lpa_time : chrono;
  (** chronometer of the total time spent in left paramodulation. *)
}


(** global monitor, uptated by the module *)
val monitor : monitor


(** [lparamodulation r c]
    @return the list of clauses obtained by 
    application of one step of left paramodulation with [r]
    in one of the negative selected equations of [c].

    The subterms introduced by instanciation are marked as {i blocked} 
    in the clauses returned. *)
val lparamodulation:
  trs -> clause -> clause list 

