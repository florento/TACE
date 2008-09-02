(** Basic narrowing *)

open Term
open Substitution
open Equation
open Trs


(** [narrow r t]
    @return the list of [(term, substitution)]
    obtained from [t] after one step of basic narrowing
    using the rewrite system [r]. 
    Terms are blocked in the subtitutions returnes.

    [r] and [t] are assumed variable disjoint. 

    Exhaustive strategy
    (all the possible narrowing steps are applied *)
val narrow:
  trs -> term -> (term * substitution) list


(** [narrow_equation r e]
    @return the list of [(equation, substitution)]
    obtained from [e] after one step of basic narrowing
    in either side of [e] using the rewrite system r. 
    Terms are blocked in the subtitutions returned.

    [r] and [e] are assumed variable disjoint. 

    Exhaustive strategy
    (all the possible narrowing steps are applied). *)
val narrow_equation:
  trs -> equation -> (equation * substitution) list


(** [solve r e]
    @return the list of solutions (as substitutions)
    of the equation [e] modulo the TRS [r].
    Computed with basic narrowing with exhaustive search strategy.
    
    {b NB:} the terms in the codomain of the solutions are
    marked as {i blocked}. *)  
val solve:
  trs -> equation -> substitution list
