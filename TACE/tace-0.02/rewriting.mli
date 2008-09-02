(** rewriting with convergent trs *)

open Term
open Trs
open Substitution
open Equation
open Trs


(** [normalize r t]
    @return the term obtained by normalizing [t] with [r].
    Rewriting strategy is leftmost, 
    innermost and with first applyable rewrite rule in [r]. *)
val normalize:
  trs -> term -> term


(** [equation_normalize r e]
    @return an equation obtained from [e] by normalization with [r]. *)
val equation_normalize:
  trs -> equation -> equation


(** [normal_form r t]
    @return true iff the term [t] is in normal form wrt [r]. *)
val normal_form:
  trs -> term -> bool




