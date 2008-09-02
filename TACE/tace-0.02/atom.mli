(** Non-equational atoms.
    An atom is a term whose top symbol is a PREDICATE or
    a SPLITTING symbol. 

    We keep information on selection along with eqach atom. *)

open Symbol
open Variable 
open Renaming 
open Term
open Substitution
open Trs



type atom


(** {2 Construction} *)


(** [make s tl]
    @return a new atom with predicate symbol [s]
    and arguments (terms) list [tl].
    The atom is marked as unselected.
    @raise Failure if [s] is not a registered symbol
    or [s] is not of kind [PREDICATE] or [SPLITTING]
    or if the arity of [s] does not correspond to the 
    length of [tl]. *)
val make:
  symbol -> term list -> atom


(** [of_string s]
    @param a string representation of an atom,
    following the syntax of {! Term.of_string},
    with a predicate symbol at top.
    @return the atom correspoding to the string [s].
    Symbols and variables of the terms are parsed
    and registered respectively with {! Symbol.of_string}
    and {! Variable.of_string}. 
  
    @raise Parsing.Parse_error in case of parse error *)
val of_string:
  string -> atom


(** [parse_atom st]
    @return the atom parsed from the char stream [st].
    @raise Parsing.Parse_error (see {! Atom.of_string} for syntax).  *)
val parse_atom:
  char Stream.t -> atom

  
(** [of_term t]
    @return the atom of the term [t]. 
    [t] must be an atom (see {! Term.is_atom}. *)
val of_term:
  term -> atom


(** [to_term a]
    @return a cast of of the atom [a] to a term. *)
val to_term:
  atom -> term




(** {2 Access} *)


(** [get_predicate a]
    @return the predicate symbol of the atom [a]. *)
val get_predicate:
  atom -> symbol


(** [get_args a]
    @return the argument list of the atom [a]. *)
val get_args:
  atom -> term list


(** [is_selected a]
    @return true iff [a] is marked as selected. *)
val is_selected:
  atom -> bool


(** [vars a]
    @return the list of variables occurring
    in the atom [a] (without repetition). *) 
val vars:
  atom -> variable list


(** [occur x a] 
    @return [true] if the variable [x] occurs in the given atom [a]. *)
val occur:
  variable -> atom -> bool


(** [size a]
    @return the size of the atom [a],
    i.e. the number of symbols and variable occurrences in [a]. *)
val size:
  atom -> int


(** [equal a1 a2]
    @return [true] if the atoms [a1] and [a2] 
    are structurally equal, return [false] otherwise. *)
val equal:
  atom -> atom -> bool



(** {2 Transformations} *)


(** [select a]
    @return a copy of the atom [a] which is marked as selected. *)
val select:
  atom -> atom


(** [unselect a]
    @return a copy of the atom [a] which is marked as not selected. *)
val unselect:
  atom -> atom


(** [rename ren a]
    @return an atom obtained from [a] by renaming 
    all its variables by [ren] or fresh variable.
    [ren] is updated. *)
val rename:
  renaming -> atom -> atom


(** [refresh a]
    @return an atom obtained from [a] by 
    renaming all its variables by fresh variables. *)
val refresh:
  atom -> atom


(** [map_var f a]
    @return a copy of the atom [a]
    where all variables have been transformed by [f].

    This is useful to colorize the variables of [a]. *)
val map_var:
  (variable -> variable) -> atom -> atom


(** [normalize r a]
    @return an atom obtained from [a] by normalization with [r]. *)
val normalize:
  trs -> atom -> atom


(** {2 Substitution} *)


(** [substitute a]
    @return the atom obtained by application of the
    given substitution [subst] to the given atom [a]. *)
val substitute:
  substitution -> atom -> atom


(** {2 Unification} *)


(** [unify a1 a2]
    @return [Some] of the mgu (as substitution)
    of the given atoms [a1] and [a2], or [None]
    if [a1] and [a2] are not unifiable. *)
val unify:
  atom -> atom -> substitution option


(** {2 Matching} *)


(** [matcher a1 a2]
    @return [Some] of the matcher for the atoms [a1] and [a2],
    or [None] if [a1] does not match [a2]. *)
val matcher:
  atom -> atom -> substitution option


(** [matching a1 a2]
    @return [true] iff [a1] matches [a2]. *)
val matching:
  atom -> atom -> bool


(** {2 Testers} *)


(** [is_deep a]
    @return [true] if [a] has at least one argument
    and the first argument of [a] is not a variable. *)
val is_deep:
  atom -> bool


(** [is_flat a]
    @return [true] iff [a] has the form [Q(x1,...,xn)]
    where [x1],...,[xn] are variables (not necessarily distinct). *)
val is_flat:
  atom -> bool


(** [is_epsilon a]
    @return [true] iff [a] has the form [Q(x)] where
    - [Q] is a symbol of kind [PREDICATE]
    - [Q] has arity 1
    - [x] is a variable. *)
val is_epsilon:
  atom -> bool


(** [is_splitting a]
    @return [true] if [a] is a splitting atom
    (i.e. its predicate is a splitting predicate). *)
val is_splitting:
  atom -> bool


(** [exists_var p a]
    @return [true] if at least one of the variables of the atom [a] 
    satisties the predicate [p]. *)
val exists_var:
  (variable -> bool) -> atom -> bool 


(** [for_all_var p a]
    @return [true] if every variable of the atom [a] 
    satisties the predicate [p]. *)
val for_all_var:
  (variable -> bool) -> atom -> bool 


(** {2 Output} *)

(** [to_string a]
    @return a string representation of the atom [a].
    The string starts with a '*' if the atom is selected. *)
val to_string:
  atom -> string


(** [check a]
    @return [true] iff [a] is a well formed atom (see {! Term.is_atom}). *)
val check:
  atom -> bool


