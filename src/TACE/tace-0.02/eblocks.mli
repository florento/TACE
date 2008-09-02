(** List of epsilon-blocks *)


open Symbol
open Variable
open Renaming
open Atom


(* List of epsilon-blocks.
   An epsilong block is a list of atoms of the form [Q1(x), ..., Qn(x)]. *)
type eblocks


(** {2 Constructors} *)


(** [make ()]
    @return an empty list of epsilon blocks. *) 
val make:
  unit -> eblocks


(** [add x q ebl]
    @return the list of epsilon-blocks obtained from [ebl]
    after insertion of an atom [Q(x)] (binding [(x, Q)]). *)
val add:
  variable -> symbol -> eblocks -> eblocks


(** [add_atom a ebl]
    @return the list of epsilon-blocks obtained from [ebl]
    after insertion of the atom [a]. 
    [a] must be of the form [Q(x)],
    where [Q] is a unary symbol of kind [PREDICATE],
    see {!Atom.is_epsilon}. *)
val add_atom:
  atom -> eblocks -> eblocks


(** [of_atoms al]
    @return the list of epsilon-blocks obtained from the list of atoms [al].
    The atoms of [al] must be of the form [Q(x)],
    where [Q] is a symbol of kind [PREDICATE],
    see {! Eblock.add_atom}. *)
val of_atoms:
  atom list -> eblocks


(** [append ebl1 ebl2]
    @return the concatenation
    of two epsilon-block lists [ebl1] and [ebl2]. *)
val append:
  eblocks -> eblocks -> eblocks



(** {2 Accessors} *)


(** [vars ebl]
    @return the list of variables of the epsilon block list [ebl]. *)
val vars:
  eblocks -> variable list


(** [mem x ebl]
    @return true iff the variable [x] occurs in 
    the epsilon block list [ebl]. *)
val mem:
  variable -> eblocks -> bool


(** [get_predicates eb]
    @return the list of predicate symbols applied to the variable [x]
    in the epsilon block list [ebl].
    [x] must occur in [ebl]. *)
val get:
  variable -> eblocks -> symbol list


(** [get_predicates eb]
    @return the list of the list of predicate in each epsilon block
    in the epsilon block list [ebl]. *)
val get_predicates:
  eblocks -> symbol list list


(** [mem_pred s ebl]
    @return true iff the symbol [s] is one of the symbols of [ebl]. *)
val mem_pred:
  symbol -> eblocks -> bool


(** [is_empty ebl]
    @return true iff the epsilon block list [ebl] is empty. *)
val is_empty:
  eblocks -> bool


(** [is_linear ebl]
    @return true iff the epsilon block list [ebl] contains
    exactly one atom [Q(x)] for each variable [x]. *)
val is_linear:
  eblocks -> bool


(** [length eb]
    @return the number of atoms [Q(x)]
    corresponding to to [eb]. *)
val length:
  eblocks -> int


(** [size eb]
    @return the size of the [eblock] [eb],
    i.e. twice the number of atoms corresp. to [eb]. *)
val size:
  eblocks -> int



(** {2 Conversion } *)


(** [to_atoms eb]
    @return the list of list of atoms corresponding to the list 
    of epsilon blocks [ebl]. *)
val to_atoms:
  eblocks -> atom list list


(** [map_var f ebl]
    @return a copy of the epsilon block list [ebl]
    where all variables have been transformed by [f].

    This is useful e.g. to color the variables of [ebl]. *)
val map_var:
  (variable -> variable) ->  eblocks -> eblocks


(** [rename ren ebl]
    @return an epsilon block list obtained from [ebl] by renaming 
    all its variables by [ren] or fresh variable.
    [ren] is updated. *)
val rename:
  renaming -> eblocks -> eblocks


(** [refresh ebl]
    @return an epsilon block list obtained from [ebl] by 
    renaming all its variables by fresh variables. *)
val refresh:
  eblocks -> eblocks


(** [to_string eb]
    @return a string representation of the list of epsilon blocks [ebl]. *)
val to_string:
  eblocks -> string
  

(** [check eb]
    @return [true] iff the epsilon-blocks list [eb] 
    is conform to its type definition.
    For debug purpose. *)
val check:
  eblocks -> bool


(** [dump e]
    print the internal representation of the epsilon-block [e]. *)
val dump:
  eblocks -> unit

