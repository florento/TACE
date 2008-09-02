(** Binary resolution *)

open Chrono
open Clause


(** monitor of the resolution module *)
type monitor = {
  mutable res_tried : int;
  (** total number of resolution steps (successful or not) *)
  mutable res_success : int;
  (** number of successful resolution steps. *)
  mutable res_fail_table : int;
  (** number of resolution steps which failed because not in the table. *)
  mutable res_fail_other : int;
  (** number of resolution steps which failed for another reason. 
      we have:
      [# success = res_tried - (res_failed_table + res_failed_other] *)     
  mutable res_time : chrono;
  (** chronometer of the total time spent in resolution. *)
}


(** global monitor, uptated by the module *)
val monitor : monitor


(** [resolution c1 c2]
    @return the clauses obtained by application of resolution
    of the clause [c1] into the clause [c2].
    @see {! Clause.select_equations}, {! Clause.select_atoms}, 
    {! Clause.select_unif}
    for the definition of the selection strategy. 

    The variables of [c1] and [c2] are assumed uncolored. *)
val resolution:
  clause -> clause -> clause list




(** {2 Strategies for resolution } *)


(** {6 Strategy for saturation: ordering}
    - [>] total on test predicate
    - any test predicate [>] any non-test predicate
    - any non-test predicate [>] any splitting symbol. 

    {b Strategy for saturation: selection.}
    - negative equations
    - splitting (negative) literals
    - deep (not flat) negative literals *)


(** {6 Categories and kinds}
    
   {b Symbol kinds.}
    We assume the following kind of predicate symbols:
    - unary predicates, of kind {! Symbol.symbol_kind} [PREDICATE],
    which are test predicates (see {! Symbol.is_testpredicate}).
    {i test predicates} are totally ordered by [<],
    - other unary predicates, of kind {! Symbol.symbol_kind} [PREDICATE],
    - nullary epsilon-splitting predicates,
    - other predicates for non-ground splitting.
    
    {b Clause types.} 
    The strategy and the form (and kind) of initial clauses
    ensure that the saturation produces only clauses 
    of types defined in {! Clausetype}.

    {b Clause kinds.}
    All the clauses obtained by saturation of a list of initial clauses as above
    are of one of these 3 following kinds:
    - [0] all the unary predicates are not test predicate,
    - [1] the head predicate is a test predicate,
    this predicate occurs exactly once in the body,
    all the other predicates in the body are not test predicate,
    - [2] the head predicate is a test predicate,
    all the test predicates (there may be none) occuring in the body
    are smaller than the head predicate w.r.t. [<]. 
 

    Below, we shall partition the set of clauses obtained by saturation, 
    each part being characteriaed by a category and a kind (integer [0..2]),
    e.g. [ONEVAR0].

    Note: [ONEVAR1] is a tautology (deleted).  *)


(** {6 Resolution steps}

    {b New clause [c] into clauses of a {!Clauseset}};
    we list below the cases of resolution of [c] into a clause [c'] of the 
    clauseset, according to {!Clausetype}:
    - [c] in [EQ], no resolution possible, but narrowing can be performed
    in the equations of [c]
    - [c] in [SPOS], [c'] is in [SPLIT], 
    and contains the nullary splitting symbol [q] in the head of [c].
    {L {e table} [tbl_symboln_SPLIT]:}
    {L symbol [q] -> 
    (index of [wclause] [c'] of type [SPLIT] in clauseset, 
    index of negative literal [q] in [c'])}
    - [c] in [REG] or [UNSEL], [c'] in [ONEVAR0] or [UGOAL] and contains 
    a negative literal [Q(x)] ([Q] head predicate of [c]).
    {L {e table} [tbl_symboln_ONEVAR]:}
    {L symbol [Q] -> 
    (index of [wclause] [c'] of type [ONEVAR] in clauseset,
    index of negative literal [Q(x)] in [c'])}
    {L {e table} [tbl_symboln_UGOAL]:}
    {L symbol [Q] -> 
    (index of [wclause] [c'] of type [UGOAL] in clauseset,
    index of negative literal [Q(x)] in [c'])}
    - [c] in [REG],   [c'] in [DEEP] (selected) and contains a negative
    literal [Q(t)], instance of head literal of [c].
    {L {e table} [tbl_symbol2n_DEEP]:}
    {L (unary predicate symbol [Q], function symbol [f]) -> 
    (index of [wclause] [c'] of type [DEEP] in clauseset, 
    index of deep negative literal with predicate [Q] 
    and topsymbol [f] in [c'])}
    (slower alternative: {! Termset.find_fastinstances} 
    and above table [tbl_posn_DEEP])
    - [c'] in [ONEVAR2]: resolution not possible because of ordered strategy
    - [c'] in [UNSEL]: resolution not possible because of ordered strategy
    - [c'] in [REG]: resolution not possible because of ordered strategy
    - [c] in [SPLIT]: resolution not possible because of selection strategy
    - [c] in [DEEP]: resolution not possible because of selection strategy
    - [c] in [ONEVAR0] or [ONEVAR2], [c'] in [ONEVAR0],
    resp. [ONEVAR2] or [UGOAL]
    {L Use above table [tbl_symboln_ONEVAR].}
    - [c] in [ONEVAR0], [c'] in [DEEP],
    {L {e table} [tbl_symboln_DEEP]:}
    {L symbol [Q] -> 
    (index of [wclause] [c'] of type [DEEP] in clauseset, 
    index of deep literal in [c'] with predicate [Q])}
    (slower alternative: {! Termset.find_fastinstances} 
    and above table [tbl_posn_DEEP]).
    - [c] in [ONEVAR2], [c'] in [DEEP]: resolution not possible 
    because of ordered strategy
    - [c] in [UGOAL]: resolution not possible because of ordered strategy
    - [c] in [UNSEL], [c'] in [DEEP] (selected) and contains a negative
    literal [Q(t)], unifiable with head literal of [c].
    [c'] found with {! Termset.find_fastunif} and
    {L {e table} [tbl_posn_DEEP]:}
    {L position [p] in [termset] -> 
    (index of [wclause] [c'] of type [DEEP] in clauseset, 
    index  in [c'] of negative deep literal of position [p])}

 
    {b Clauses of a {!Clauseset} into a new clause [c]};
    we list below the cases of resolution of a clause [c'] of the clauseset
    into [c], according to {!Clausetype}:

    - [c] in [EQ]: narrowing into the equations of [c]
    - [c'] in [EQ]: no resolution possible
    - [c] in [REG]: resolution not possible because of ordered strategy    
    - [c'] in [SPOS], [c] is in [SPLIT], 
    and contains the nullary splitting symbol [q] in the head of [c'].
    {L {e table} [tbl_symbolp_SPOS]:}
    {L symbol [q] -> 
    index of [wclause] [c'] of type [SPOS] in clauseset 
    with positive literal [q] in [c']}
    (index kept for history construction)
    - [c'] in [REG], [c] in [DEEP] (selected) and contains a negative
    literal [Q(t)], instance of the positive literal of [c'].
    {L {e table} [tbl_symbol2p_REG]:}
    {L (unary predicate symbol [Q], function symbol [f]) -> 
    index of [rclause] [c'] of type [REG] in clauseset
    with head predicate [Q] and head topsymbol [f]}
    - [c'] in [ONEVAR], [c] in [DEEP0] or [DEEP1]
    and contains deep negative literal with top predicate [Q] of [c']
    {L {e table} [tbl_symbolp_ONEVAR]:}
    {L symbol [Q] -> 
    index of clause [c'] of type [ONEVAR] in clauseset
    with predicate [Q] in positive literal}
    - [c'] in [UNSEL], [c] in [DEEP] (selected) and contains a negative
    literal [Q(t)], unifiable with head literal of [c].
    [c'] found with {! Termset.find_fastunif} and
    {L {e table} [tbl_posp_UNSEL]:}
    {L position [p] in [termset] -> 
    index of [wclause] [c'] of type [UNSEL] in clauseset
    whose head is the atom at position [p] in termset}    
    - [c'] in [REG] or [UNSEL], [c] in [ONEVAR0] or [UGOAL] 
    and contains a negative
    literal [Q(x)] ([Q] head predicate of [c']).
    {L {e table} [tbl_symbolp_REG]:}
    {L symbol [Q] -> 
    index of [wclause] [c'] of type [REG] in clauseset
    with predicate [Q] in positive literal}
    {L {e table} [tbl_symbolp_UNSEL]:}
    {L symbol [Q] -> 
    index of [wclause] [c'] of type [UNSEL] in clauseset
    with predicate [Q] in positive literal}
    - [c'] in [ONEVAR0] or [ONEVAR2], [c] in [ONEVAR0] or [UGOAL]
    and contains a negative literal [Q(x)] ([Q] head predicate of [c']).
    {L {e table} [tbl_symbolp_ONEVAR]:}
    {L symbol [Q] -> 
    index of [wclause] [c'] of type [ONEVAR] in clauseset
    with predicate [Q] in positive literal}
    - [c] in [ONEVAR2]: resolution not possible because of ordered strategy
    - [c] in [UNSEL]: resolution not possible because of ordered strategy
    - [c'] in [SPLIT]: resolution not possible because of selection strategy
    - [c'] in [DEEP]: resolution not possible because of selection strategy
    - [c'] in [UGOAL]: resolution not possible because of ordered strategy *)

