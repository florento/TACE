
(** Clauses and selection *)

(** We guaranty that all the clauses returned by functions
    of this module have no redundant negative or positive literals. *)


open Symbol
open Variable
open Atom
open Equation
open Eblocks
open Clausetype
open Substitution

type sign = 
    POS
  | NEG;;


type clause


(** {2 Construction} *)


(** [make nsl nal nel psl pal pel]
    @return a clause with 
    - the negative splitting literals listed in [nsl],
    - the negative non-splitting literals listed in [nal],
    - the negative equational literals listed in [nel],
    - the positive splitting literals listed in [psl],
    - the positive non-splitting literals listed in [pal],
    - the positive equational literals listed in [pel]. *)
val make:
   atom list -> atom list -> equation list -> 
   atom list -> atom list -> equation list -> clause


(** [empty ()]
    @return an empty clause. *)
val empty:
  unit -> clause

(** [of_string s]
    @param a string representation of an equational clause:
    - list of atoms separated by ',' 
    - followed by '=>' and 
    - followed by a (posibly empty) list of atoms and
    - terminated by '.'.
    Every non equational atom has the form required by {! Atom.of_string}
    and every equation has the form required by {! Equation.of_string}.
    An empty optional positive atom is characterized by two 
    newlines after '=>' (there might be spaces inbetween).
    The spaces, tabs and newlines are ignored.   

    @return the clause corresponding to the string [s].
    Symbols and variables of the terms are parsed
    and registered respectively with {! Symbol.of_string}
    and {! Variable.of_string}. 
  
    @raise Parsing.Parse_error if parsing of the literals fails. *)
val of_string:
  string -> clause


(** [parse_clause st]
    @return the clause parsed from the char stream [st].
    @raise Parsing.Parse_error see {! Clause.of_string} for syntax. *)
val parse_clause:
  char Stream.t -> clause


(** [merge c1 c2]
    @return a clause with all the negative literals of [c1] and
    all the literals of [c2] (usefull for resolution).

    NB: The {! Clausetype.clausetype} of the resulting clause can be UNDEF
    if this function is applied without further epsilon-splitting. *)
val merge:
  clause -> clause -> clause


(** [cast c]
    @return a new clause with the same contents as [c]
    and with updated type and selection. *)
val cast:
  clause -> clause



(** {2 Access} *)


(** [size c]
    @return the size of the [clause] [c]. *)
val size:
  clause -> int


(** [get_type c]
    @return the size of the [clause] [c]. *)
val get_type:
  clause -> clausetype


(** [length c]
    @return the number of literals in [c].  *)
val length:
  clause -> int


(** [lengths c]
    @return the number of literals of sign [s] in [c].  *)
val lengths:
  sign -> clause -> int



(** [get_equations s c]
    @return the list of negative or positive (according to the sign [s])
    equations of [c]. *)
val get_equations:
  sign -> clause -> equation list


(** [get_atoms s c]
    @return the list of negative or positive (according to the sign [s])
    atoms of [c]. *)
val get_atoms:
  sign -> clause -> atom list


(** [get_splittings s c]
    @return the list of negative or positive (according to the sign [s])
    splitting literals of [c]. *)
val get_splittings:
  sign -> clause -> atom list


(** [get_others s c]
    @return the list of negative or positive (according to the sign [s])
    non-splitting and non-equational atoms of [c]. *)
val get_others:
  sign -> clause -> atom list


(** [to_equation c]
    @return the positive and equational literal of [c],
    assuming [c] satisfy {! Clause.is_equation}.  *)
val to_equation:
  clause -> equation



(** {2 Access: special for regular tree automata transitions} *)


(** [reg_get_target c]
    @return the target state of the regular clause [c].
    [c] must be of {! Clausetype.REG}. *)
val reg_get_target:
  clause -> symbol


(** [reg_get_function c]
    @return the read function symbol of the regular clause [c].
    [c] must be of {! Clausetype.REG}. *)
val reg_get_function:
  clause -> symbol


(** [reg_get_args c]
    @return the list of state arguments of the regular clause [c].
    [c] must be of {! Clausetype.REG}. *)
val reg_get_args:
  clause -> symbol list



(** {2 Testing} *)


(** [is_horn c]
    @return [true] iff the clause [c] is a Horn clause. *)
val is_horn: 
  clause -> bool


(** [is_positive c]
    @return [true] iff the clause [c] has no negative literals
    and a non-empty head. *)
val is_positive: 
  clause -> bool


(** [is_empty c]
    @return [true] iff the clause [c] has no literals,
    return [false] otherwise. *)
val is_empty:
  clause -> bool


(** [is_goal c]
    @return [true] if the clause [c] has no positive literals,
    return [false] otherwise. *)
val is_goal:
  clause -> bool


(** [is_goal_or_splitting c]
    @return [true] if the clause [c] has an empty head
    or a head which is a splitting predicate,
    return [false] otherwise. *)
val is_goal_or_splitting:
  clause -> bool


(** [is_regular c]
    @return [true] iff the clause [c] is regular
    (see {! Clausetype.REG}.). *)
val is_regular:
  clause -> bool


(** [is_equation c]
    @return [true] iff the clause [c] has only one literal
    and this literal is positive and equational. *)
val is_equation:
  clause -> bool


(** {2 Utilities} *)


(** [for_all atom_test eq_test c]
    @return true iff [atom_test] is true for every non-equational atom of [c]
    and [eq_test] is true for every equational atom of [c]. *)
val for_all:
  (atom -> bool) -> (equation -> bool) -> clause -> bool


(** [for_all atom_filter eq_filter c]
    @return a clause build with 
    all non-equational atoms of [c] for which [atom_filter] is true
    and all equational atoms of [c] for which [eq_filter] is true. *)
val filter:
  (atom -> bool) -> (equation -> bool) -> clause -> clause



(** {2 Variable utilities} *)


(** [refresh c]
    @return the clause obtained from the the clause [c]
    by renaming all its variables to the variables 
    of index 1..n and without color. *)
val refresh :
  clause -> clause 


(** [for_all_var f c]
    @return true iff all the variables of the clause [c]
    satisfy the test [f]. *)
val for_all_var:
  (variable -> bool) -> clause -> bool


(** [map_var f c]
    @return a copy of the clause [c]
    where all variables have been transformed by [f].

    This is useful to colorize the variables of [c].
    
    NB: The {! Clausetype.clausetype} of the resulting clause can be UNDEF
    if this function is applied without further epsilon-splitting.  *)
val map_var:
  (variable -> variable) -> clause -> clause


(** {2 Substitution} *)


(** [substitution subst c]
    @return the clause obtained by application of the
    given substitution [subst] to the atoms of the clause [c]. *)
val substitute:
  substitution -> clause -> clause




(** {2 Selection} *)

(** We implement the following selection strategy:
    - if there are some equations, select all of them
    - if there are no equations, 
    if there are some splitting literals, 
    select all of them
    - if there are no equations, 
    if there are no splitting literals, 
    select one of the deep non-splitting literals. *)


(** [select_equations c] 
    @return a list of pairs [(e, c')] where
    [e] is a (selected) negative equation of [c] 
    and [c'] is the rest of the clause [c]. *)
val select_equations:
  clause -> (equation * clause) list


(** [select_atoms c] 
    @return a list of pairs [(a, c')] where
    [a] is a selected negative literal of [c] 
    and [c'] is the rest of the clause [c].
    
    NB: [c'] must be casted. *)
val select_atoms:
  clause -> (atom * clause) list


(** [select_atoms_unif b c] 
    @return a list of triples [(a, c', s)] where:
    - [a] is a selected negative literal of [c] unifiable with [b]
    - [c'] is the rest of the clause [c] ([c] without [a])
    - [s] is the mgu of [a] and [b]. 

    NB: [c'] must be casted. *)
val select_atoms_unif:
  atom -> clause -> (substitution * clause) list



(** {2 Epsilon-splitting} *)


(** [extract_eblocks c]
    @return a pair made of a list of epsilon blocks extracted from [c]
    and the rest of the clause. *)
val extract_eblocks:
  clause -> eblocks * clause;;




(** {2 Output} *)


(** [to_string c]
    @return a string representation of the clause [c]. *)
val to_string: 
  clause -> string


(** [to_stringt ts c]
    @return a string representation of the [tclause] [c] along with its
    clausetype. Cannot be parsed by {! Eclause.of_string}. *)
val to_stringt:
  clause -> string


(** [dump c]
    @return print the internal representation of the clause [c]. *)
val dump: 
  clause -> unit


(** [check c]
    @return [true] iff DEPRECATED
    - every symbol in [c.w_split] has kind [SPLITTING] and arity [0],
    - the epsilon block list [c.w_block] is well formed,
    - every position of [c.w_deep] points to an atom in [ts],
    - [c.w_head] passes {! Whead.check},
    - the recorded size [c.w_size] corresponds to the size of [c].

    For debug purposes.
    see {!Eblock.check},
    see {!Termset.is_atom} *)
val check:
  clause -> bool

