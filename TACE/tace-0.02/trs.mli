(** Term Rewriting Systems and equational theories *)

open Variable
open Equation


type trs =
    equation list;;


(** [make ()]
    @return a new empty trs. *)
val make:
  unit -> trs


(** [add e r]
    @return a trs made by adding the equation [e]
    at the beginning of [r]. *)
val add:
  equation -> trs -> trs


(** [is_empty r]
    @return true iff [r] is an empty trs. *)
val is_empty:
  trs -> bool


(** [length r]
    @return the number of equations in the trs [r]. *)
val length:
  trs -> int


(** [map_var f r]
    @return a copy of the trs [r]
    where all variables have been transformed by [f].

    This is useful to colorize the variables of [r]. *)
val map_var:
  (variable -> variable) -> trs -> trs


(** [map f r]
    @return the list obtained by application
    of [f] to the equations of the trs [r]. *)
val map:
  (equation -> 'a) -> trs -> 'a list


(** [to_string r]
@return a string representation of the TRS [r]. *)
val to_string:
  trs -> string


(** [check r]
    @return true iff the trs [r] is conform to its definition.
    For debug purpose. *)
val check:
  trs -> bool


