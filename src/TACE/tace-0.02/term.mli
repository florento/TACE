(** Terms *)

open Symbol
open Variable
open Renaming


(** A term is either a {! Variable.variable} or
    a {! Symbol.symbol} (of any kind) followed by a list of terms
    whose length is equal to the arity of the symbol. *)
type term =
    T of symbol * term list
  | V of variable


(**********************)
(** {2 Constructors } *)   
(**********************)


(** [make s tl]
    @return a new term with top symbol [s]
    and subterms list [tl].
    [s] must be have been registered
    with {! Symbol.make} or {! Symbol.of_string}.
    @raise Failure if [s] is not a registered symbol
    or if the arity of [s] does not correspond to the 
    length of [tl]. *)
val make:
  symbol -> term list -> term


(** [of_variable v]
    @return a new term made of the given variable [v].
    Does not check wether [v] is really a registered variable. *)
val of_variable:
  variable -> term


(** [of_string s]
    @param a string representation of a term,
    containing '(', ')', separator ','
    and identifier for symbols and variables 
    following the syntax defined by the functions
    {! Symbol.of_string} and {! Variable.of_string}.
    The spaces in [s] are ignored (except in indentifiers).

    @return a term correspoding to the string [s].
    Symbols and variables of the terms are parsed
    and registered respectively with {! Symbol.of_string}
    and {! Variable.of_string}. 
  
    @raise Parsing.Parse_error in case of parse error *)
val of_string:
  string -> term


(** [parse_term st]
    @return the term parsed from the char stream [st].
    @raise Parsing.Parse_error if error.  *)
val parse_term:
  char Stream.t -> term


(** [parse_args st]
    @return the list of terms parsed from the char stream [st].
    Separator is ',', blank are ignored, 
    the end of args list is marked by ')'.
    @raise Parsing.Parse_error if error.  *)
val parse_args:
  char Stream.t -> term list


(******************)
(** {1 Accessors} *)
(******************)


(** [topsymbol t]
    @return the top symbol of the term [t].
    @raise Failure if [t] is a variable. *)
val topsymbol:
  term -> symbol


(** [subterms t]
    @return the list of direct subterms of [t]
    and the empty list if [t] is a variable. *)
val subterms:
  term -> term list


(** [vars t]
    @return the list of variables occurring
    in the term [t] (without repetition). *) 
val vars:
  term -> variable list


(** [size t]
    @return the size of the term [t],
    i.e. the number of symbols and variable occurrences in [t]. *)
val size:
  term -> int


(** [equal t1 t2]
    @return [true] if the terms [t1] and [t2] 
    are structurally equal, return [false] otherwise. *)
val equal:
  term -> term -> bool


(*****************)
(**  {1 Utils}   *)
(*****************)


(** [is_variable t]
    @return [true] if the given term [t]
    is a variable, return [false] otherwise. *)
val is_variable:
  term -> bool


(** [to_variable t]
    @return a variable if [t] is a variable
    @raise Failure if [t] is not a variable. *)
val to_variable:
  term -> variable


(** [is_atom t]
    @return [true] if the term [t] is an atom,
    i.e. if its top symbol is a predicate
    (kind [PREDICATE] or [SPLITTING])
    and its internal symbols have kind [FUNCTION]. 
    return [false] otherwise. *)
val is_atom:
  term -> bool 


(** [is_ground t]
    @return [true] if the given term [t]
    has no variable, return [false] otherwise. *)
val is_ground:
  term -> bool


(** [occur x t] 
    @return [true] if the variable [x]
    occurs in the given term [t], and return [false] otherwise. *)
val occur:
  variable -> term -> bool


(** [is_flat t]
    @return [true] if [t] has the form [f(x1,...,xn)]
    where [x1],...,[xn] are variables (not necessarily distinct).
    return [false] otherwise. *)
val is_flat:
  term -> bool


(** [is_linear t]
    @return [true] if no variable occurs more than once in [t].
    return [false] otherwise. *)
val is_linear:
  term -> bool


(** [check t]
    @return true iff the term [t] is conform to its definition. 
    For debug purpose. *) 
val check:
  term -> bool



(********************************************)
(**  {1 Variable tagging and substitution}  *)
(********************************************)

(** [map_var f t]
    @return a copy of the term [t]
    where all variables have been transformed by [f].

    This is useful to color the variables of [t]
    when it is assumed to be variable disjoint with another term. *)
val map_var:
  (variable -> variable) -> term -> term


(** [exists_var p t]
    @return [true] if at least one of the variables of [t] 
    satisties the predicate [p]. *)
val exists_var:
  (variable -> bool) -> term -> bool 


(** [for_all_var p t]
    @return [true] if every variable of [t] 
    satisties the predicate [p]. *)
val for_all_var:
  (variable -> bool) -> term -> bool 


(** [rename ren t]
    @return an term obtained from [t] by renaming 
    all its variables by [ren] or fresh variable.
    [ren] is updated. *)
val rename:
  renaming -> term -> term


(** [refresh t]
    @return a term obtained from [t] by 
    renaming all its variables by fresh variables. *)
val refresh:
  term -> term



(*******************************************)
(**  {1 Blocking for basic strategies}     *)
(*******************************************)

(** [block t]
    @return the term [t] after marking all its symbols as {i blocked}.
    Uses the flag1 of the symbols to mark them. *)
val block:  
  term -> term


(** [unblock t]
    @return the term [t] after marking all its
    symbols as {i not blocked}.  *)
val unblock: 
  term -> term


(** [is_blocked t]
    @return [true] is the top symbol of the term [t] 
    is marked as blocked or if [t] is a variable,
    return [false] otherwise. *)
val is_blocked: 
  term -> bool 


(****************)
(** {1 Output}  *)
(****************)

(** [to_string t]
    @return a string representation of the given term [t]. *)
val to_string:
  term -> string




