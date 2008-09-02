(** Execution of instructions *)

open Chrono
open Clauseset
open Instruction
open Clause


(** monitor of the module interpreter *)
type monitor = {
  mutable interp_eager_time : chrono;
  (** chronometer of the time spent in application of eager inferences. *)
}


(** global monitor, uptated by the module *)
val monitor : monitor


(** {2 non eager inferences} *)


(** [exec_nil cs]
    @return the empty list. *)
val exec_nil: 
  clauseset -> (clause * instruction) list


(** [exec_lparamodulation cs c i]
    @return the list of pairs made of a clause and the instruction
    producing the clause in [cs]
    obtained by  solving, with the trs in [cs]
    the negative equations selected 
    in the given clause [c] of index [i] in [cs]. *)
val exec_lparamodulation:
  clauseset -> clause -> int -> (clause * instruction) list


(** [exec_solve cs c i]
    @return the list of pairs made of a clause and the instruction
    producing the clause in [cs]
    obtained by  solving, with the trs in [cs]
    all the negative equations in the given clause [c] of index [i] in [cs]. *)
val exec_solve:
  clauseset -> clause -> int -> (clause * instruction) list


(** [exec_rparamodulation cs c i]
    @return the list of pairs made of a clause and the instruction
    producing the clause in [cs]
    obtained by right-paramodulation into the head of the given 
    clause [c] of index [i] in [cs]
    with the equations of the trs in [cs]. *)
val exec_rparamodulation:
  clauseset -> clause -> int -> (clause * instruction) list


(** [exec_new cs c]
    @return the list (of length 1) 
    of pairs made of a clause and the instruction
    producing the clause in [cs] obtained by 
    introduction of the new clause [c] in [cs]. *)
val exec_new:
  clauseset -> clause -> (clause * instruction) list


(** [exec_resolutions cs c1 i1 c2 i2]
    @return the list of pairs made of a clause and the instruction
    producing the clause in [cs] obtained by resolution of
    the clause [c1] on index [i1] in [cs]
    into the clause [c2] on index [i2] in [cs]. *)
val exec_resolution:
  clauseset -> clause -> int -> clause -> int -> (clause * instruction) list


(** [exec_resolutions cs c i]
    @return the list of pairs made of a clause and the instruction
    producing the clause in [cs]
    obtained by resolution between the clause [c] on index [i] in [cs]
    and the clauses of [cs]. *)
val exec_resolutions:
  clauseset -> clause -> int -> (clause * instruction) list


(** {2 eager inferences} *)

(** [eager cs cl]
    @return the list of clause obtained by 
    application of the following inferences to every clause in [cl]:
    - tautology deletion
    - elim of trivial equational literals
    - e-splitting 
    - TODO: non-ground splitting (optimisation).  *)
val eager:
  clauseset -> (clause * instruction) list -> (clause * instruction) list


(** {2 instructions execution} *)

(** [exec cs i]
    @return the list of clauses
    obtained by execution of the instruction [i] in the clauseset [cs].
    It can be the empty list (e.g. if one of the clause involved in [ins] 
    has been deleted in [cs]) or a singleton list. *)
val exec:
  clauseset -> instruction -> clause list


(** [recover cs i]
    @return the clause of index [i] in the clauseset [cs],
    if this clause has been deleted in [cs],
    it is recomputed according to the stored instruction. *)
val recover:
  int -> clauseset -> clause


