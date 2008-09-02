(** Variable renaming *)

open Variable


(** A variable renaming associates variables to fresh variables. *)
type renaming


(** [make ()]
    @return a new empty variable renaming. *)
val make:
  unit -> renaming


(** [mem x ren]
    @return [true] iff [ren] is defined on the variable [x]. *)
val mem:
  variable -> renaming -> bool


(** [rename ren x]
    @return the image of [x] under [ren] if [ren] is defined on [x],
    otherwise, a fresh (wrt [ren]) uncolored variable.
    In this last case, [ren] is updated. *)
val apply:
  renaming -> variable -> variable

