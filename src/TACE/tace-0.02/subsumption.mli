
(** Sumbsumption for non-equational clauses *)

open Chrono
open Clause



(** monitor of the module *)
type monitor = {
  mutable sub_tested : int;
  (** total number of subsumption tests (successful or not) *)
  mutable sub_success : int;
  (** number of successful resolution steps. *)
  mutable sub_fail_filter : int array;
  (** number of resolution steps which failed by filtering. *)
  mutable sub_fail_other : int;
  (** number of resolution steps which failed after filters. *)
  mutable sub_time : chrono;
  (** chronometer of the total time spent in subsumption. *)
}


(** global monitor, uptated by the module *)
val monitor : monitor


(** [subsume c1 c2]
    @return [true] if the clause [c1] subsumes the clause [c2]. *)
val subsume:
  clause -> clause -> bool

