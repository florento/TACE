(** Elimination of trivial literals *)

(** We assume that the clause do not contain double literals. 
    We eliminate trivial equations. *)

open Clause
  

(** [elim c]
    @return a clause obtained from [c] by elimination
    of trivial equations [t = t]. *)
val elim:
  clause -> clause

