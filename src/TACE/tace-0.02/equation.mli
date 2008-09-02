
(** Equations *)

open Variable
open Renaming
open Term
open Substitution


type equation = 
    term * term


(** {2 Construction} *)

(** [make t1 t2]
    @return the equation [t1 = t2] *)
val make:
  term -> term -> equation

  
(** [of_string s]
    @param a string representation of an equation,
    of the form [term = term]
    where each term has the form required by {! Term.of_string}.
    The spaces before and after [=] are ignored.

    @return the equation corresponding to the string [s].
    Symbols and variables of the terms are parsed
    and registered respectively with {! Symbol.of_string}
    and {! Variable.of_string}. 
  
    @raise Parsing.Parse_error in case of parse error *)
val of_string:
  string -> equation


(** [parse_equation st]
    @return the equation parsed from the char stream [st].
    @raise Parsing.Parse_error see {! Equation.of_string}
    for the syntax. *)
val parse_equation:
  char Stream.t -> equation


(** {2 Access} *)


(** [left e]
    @return the left member of the equation [e]. *)
val left:
  equation -> term


(** [right e]
    @return the left member of the equation [e]. *)
val right:
  equation -> term


(** {2 Transformation} *)


(** [map_var f e]
    @return a copy of the equation [e]
    where all variables have been transformed by [f].

    This is useful to colorize the variables of [e]. *)
val map_var:
  (variable -> variable) -> equation -> equation


(** [rename ren e]
    @return an equation obtained from [e] by renaming 
    all its variables by [ren] or fresh variable.
    [ren] is updated. *)
val rename:
  renaming -> equation -> equation


(** [refresh e]
    @return an equation obtained from [e] by 
    renaming all its variables by fresh variables. *)
val refresh:
  equation -> equation


(** {2 Substitution} *)


(** [substitute subst e]
    @return the equation obtained by application of the
    given substitution [subst] to both sides of the equations [e]. *)
val substitute:
  substitution -> equation -> equation


(** {2 Matching} *)

(** [matcher e1 e2]
    @return [Some] of the matcher for the equations [e1] and [e2],
    or [None] if [e1] does not match [e2]. 
    (we do NOT assume commutativity of equality here, 
     since equations are assumed oriented [AV]). *)
val matcher:
  equation -> equation -> substitution option


(** [matching t1 t2]
    @return [true] iff [e1] matches [e2]. *)
val matching:
  equation -> equation -> bool




(** {2 Testing} *)


(** [trivial e]
    @return [true] iff the equation [e] is of the form [t = t]. *)
val trivial:
  equation -> bool


(** [exists_var p e]
    @return [true] if at least one of the variables of [e] 
    satisties the predicate [p]. *)
val exists_var:
  (variable -> bool) -> equation -> bool 


(** [for_all_var p e]
    @return [true] if every variable of [e] 
    satisties the predicate [p]. *)
val for_all_var:
  (variable -> bool) -> equation -> bool 


(** [equal e1 e2]
    @return [true] iff the equations [e1] and [e2] are structurally equal.
    (we do NOT assume commutativity of equality here, 
     since equations are assumed oriented [AV]). *)
val equal:
  equation -> equation -> bool


(** [size e]
    @return the size of the given equation [e],
    defined as the sum of the respective size of each of its members
    plus one (size of the symbol '='). *)
val size:
  equation -> int


(** [occur x e] 
    @return [true] iff the variable [x] occurs in the given equation [e]. *)
val occur:
  variable -> equation -> bool


(** {2 Output and debug} *)


(** [to_string e]
    @return a string representation of the equation [e]. *)
val to_string:
  equation -> string


(** [check e]
    @return [true] iff the [equation] [e] is conform to its type definition.
    For debug purposes. *)
val check:
  equation -> bool

