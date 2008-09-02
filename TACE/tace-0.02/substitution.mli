(** Term substitutions and variable renamings *)

open Variable
open Term


(** A substitution is a list of binding
    associating a term to a variable. *)
type substitution 

(*******************************************)
(**  {2 Constructors}                      *)
(*******************************************)

(** [make ()]
    @return an empty substitution. *)
val make:
  unit -> substitution


(** [add x t s]
    @return a substitution obtained from [s] by adding the 
    binding of [x] to [t].
    [x] must not be bound in [s]. *)
val add:
  variable -> term -> substitution -> substitution


(*******************************************)
(**  {2 Accessors}                         *)
(*******************************************)

(** [domain subst]
    @return the domain of [subst],
    i.e. the list of variables [x] such that
    [subst] contains a binding [(x, t)]. *)
val domain:
  substitution -> variable list


(** [image subst]
    @return the image of [subst],
    i.e. the list of terms [t] such that
    [subst] contains a binding [(x, t)]. *)
val image:
  substitution -> term list


(** [mem x subst]
    @return [true] is the given variable [x]
    is in the domain of [subst],
    return [false] otherwise. *)
val mem:
  variable -> substitution -> bool


(** [assoc x subst]
    @return the image of [x] by the substitution [subst].
    [x] must be in the domain of [subst]. *)
val assoc:
  variable -> substitution -> term


(** [equal s1 s2]
    @return wether the two substitutions [s1] and [s2] are equal.
    The substitutions must be clean 
    (without two binding of the same variable). *)
val equal:
  substitution -> substitution -> bool



(** [is_triangle s]
    @return true iff the substitution [s] is in triangle form
    [x1 = t1,..., xn = tn] where does not contain variables of 
    [xi+1,..., xn]. *)
val is_triangle:
  substitution -> bool


(** [length subst]
    @return the number of variables in the domain of [subst]. *)
val length:
  substitution -> int


(** [size subst]
    @return the sum of the respective sizes of 
    the terms in the image of [subst]. *)
val size:
  substitution -> int
  

(*******************************************)
(**  {2 Application}                       *)
(*******************************************)


(** [apply subst x]
    @return the term obtained by application of the
    given substitution [subst] to the given variable [x]. 
    It is the term bound to [x] in [subst] if 
    [x] is in the domain of [subst] and
    the variable [x] as a term. *)
val apply:
  substitution -> variable -> term


(** [substitute subst t]
    @return the term obtained by application of the
    given substitution [subst] to the given term [t]. *)
val substitute:
  substitution -> term -> term


(*******************************************)
(**  {2 Testing}                           *)
(*******************************************)

(** [for_all_domain p s]
    @return trus iff all the variables in the domain of [s]
    satisfy the predicate [p]. *)
val for_all_domain:
  (variable -> bool) -> substitution -> bool


(** [for_all_image p s]
    @return trus iff all the terms in the image of [s]
    satisfy the predicate [p]. *)
val for_all_image:
  (term -> bool) -> substitution -> bool



(*******************************************)
(**  {2 Operations}                        *)
(*******************************************)

(** [map f s]
    @return the substitution obtain from [s]
    by applying [f] to all the terms in the image of [s]. *)
val map:
  (term -> term) -> substitution -> substitution


(** [minus subst1 subst2]
    @return the substitution obtained from [subst1]
    by removing any binding [(x, t)] of [subst2]
    such that [x] is in the domain of [subst1] *)
val minus:
  substitution -> substitution -> substitution


(** [compose subst1 subst2]
    @return the composition of [subst1] by [subst2],
    i.e. the substitution  [subst2 o subst1]. *)
val compose:
  substitution -> substitution -> substitution


(** [disjoint_compose subst1 subst2]
    @return the composition of [subst1] by [subst2],
    for the special case where the domains of
    [subst1] and [subst2] are disjoint. *)
val disjoint_compose:
  substitution ->   substitution ->   substitution


(** [compatible s1 s2]
    @return true iff [s1] and [s2] are compatible
    (they return the same term on same variables. *)
val compatible:
  substitution -> substitution -> bool


(** [union s1 s2]
    @return the union of [s1] and [s2].
    It is meant to be applied to compatible substitutions
    (see {! Substitution.compatible}).
    If some variable is bound both in [s1] and [s2],
    only the binding is [s2] is kept. *)
val union:
  substitution -> substitution -> substitution


(** [uniono s1 s2]
    @return [Some] of the union of [s1] and [s2] if they are compatible
    (see {! Substitution.compatible})
    and [None] otherwise. *)
val uniono:
  substitution -> substitution -> substitution option
  


(*******************************************)
(**  {2 Variables operations}              *)
(*******************************************)


(** [map_var f s]
    @return a copy of the substitution [s]
    where all variables (of image AND domain) 
    have been transformed by [f]. *)
val map_var:
  (variable -> variable) -> substitution -> substitution



(*******************************************)
(**  {2 Blocking for basic strategies}     *)
(*******************************************)

(** [block t]
    @return the substitution [t] after marking all 
    the symbols of the terms in its image  as {i blocked}.
    Uses the flag1 of the symbols to mark them.
    @deprecated use {Substitution.map} and {Term.block}. *)
val block:  
  substitution -> substitution


(** [block t]
    @return the substitution [t] after marking all 
    the symbols of the substitutions in its image  as {i not blocked}.
    Uses the flag1 of the symbols to mark them. 

    @deprecated use {Substitution.map} and {Term.unblock}. *)
val unblock: 
  substitution -> substitution



(*******************************************)
(** {2 Term conversion}                    *)
(*******************************************)


(** [to_term subst]
    @return a term representation of the substitution [subst]. 

    Useful to generalise term manipulating function to subsitutions.

    Uses special binary function symbols ["_cons"] (binary)
    ["_null"] (constant) and ["_bind"] (binary).
    An individual binding [(x, t)] is represented by the term
    [T (_bind, [V x; t])]. 
    List of bindings (substitutions) are constructed with ["_cons"]. *)
val to_term:
  substitution -> term


(** [of_term t]
    @return the substitution corresponding to the term [t],
    as defined in {!Substitution.to_term}. 
    @raise Failure in case of parse error *)
val of_term:
  term -> substitution



(*******************************************)
(** {2 Output}                             *)
(*******************************************)


(** [to_string subst]
@return a string representation of the substitution [subst]. *)
val to_string:
  substitution -> string


(** [checkfunction s]
    @return [true] iff in the substitution [s],
    every variable is defined only once. *)
val check_function:
  substitution -> bool


(** [check s]
    @return true iff the substitution [s] is conform to its definition. 
    For debug purpose. *) 
val check:
  substitution -> bool

