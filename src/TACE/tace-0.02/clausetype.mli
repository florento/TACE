(** Types of clauses *)


(** Mutually exclusive categories of clauses *)
type clausetype = 
    UNDEF
      (** [UNDEF] unknown clause type *)
  | EQ
      (** [EQ] clause with negative equations *)
  | SPOS
      (** [SPOS] positive clause with nullary splitting atom: 
          [=> q] *)
  | REG
      (** [REG]  standard tree automata clause:
	  [Q1(x1),...,Qn(xn) => Q(fx1,...,xn)] *)
  | SPLIT
      (** [SPLIT] clause with at least one negative (selected)
	  splitting literal *)
  | DEEP
      (** [DEEP] clause with at least one deep (selected) literal 
	  and not in [SPLIT] *)
  | ONEVAR
      (** [ONEVAR] clauses of the form: [Q1(x), ..., Qn(x) => Q(x)], 
	  [n >= 0].  *)
  | UGOAL
      (** [UGOAL] unselected goal clause 
	  (without head or whose head is a splitting atom)
	  of the form: 
	  [Q1(x),...,Qn(x) => q] or [Q1(x), ..., Qn(x) => ] *)
  | UNSEL
      (** [UNSEL] clauses of the form: 
          [Q1(x1), ..., Qn(xn) => Q(s)]
	  and not in [REG] or [ONEVAR] ([s] not a variable) *)



(** {2 Strategies} *)

(** 
    - basic
    - ordered 
    - selection
*) 


(** {2 Strategies: Left paramodulation steps} *)

(** 
    Left paramodulation is only possible into clauses of 
    their {!Clausetype.clausetype} [EQ].
*)



(** {2 Strategies: Resolution steps} *)

(**
   We list below the cases of resolution of a clause [c] into a clause [c'], according to 
   their {!Clausetype.clausetype}:
   - [c] in [EQ], no resolution possible, but narrowing can be performed
   in the equations of [c]
   - [c] in [SPOS], [c'] is in [SPLIT], 
   and contains the nullary splitting symbol [q] in the head of [c].
   - [c] in [REG] or [UNSEL], [c'] in [ONEVAR0] or [UGOAL] and contains 
   a negative literal [Q(x)] ([Q] head predicate of [c]).
   - [c] in [REG],   [c'] in [DEEP] (selected) and contains a negative
   literal [Q(t)], instance of head literal of [c].
   - [c'] in [ONEVAR2]: resolution not possible because of ordered strategy
   - [c'] in [UNSEL]: resolution not possible because of ordered strategy
   - [c'] in [REG]: resolution not possible because of ordered strategy
   - [c] in [SPLIT]: resolution not possible because of selection strategy
   - [c] in [DEEP]: resolution not possible because of selection strategy
   - [c] in [ONEVAR0] or [ONEVAR2], [c'] in [ONEVAR0],
   resp. [ONEVAR2] or [UGOAL]
   - [c] in [ONEVAR0], [c'] in [DEEP],
   - [c] in [ONEVAR2], [c'] in [DEEP]: resolution not possible 
   because of ordered strategy
   - [c] in [UGOAL]: resolution not possible because of ordered strategy
   - [c] in [UNSEL], [c'] in [DEEP] (selected) and contains a negative
   literal [Q(t)], unifiable with head literal of [c].
*)

 
(**
   We list below the cases of resolution of a clause [c'] of the clauseset
   into [c], according to {!Clausetype.clausetype}.
   
   - [c] in [EQ]: narrowing into the equations of [c]
   - [c'] in [EQ]: no resolution possible
   - [c] in [REG]: resolution not possible because of ordered strategy    
   - [c'] in [SPOS], [c] is in [SPLIT], 
   and contains the nullary splitting symbol [q] in the head of [c'].
   - [c'] in [REG], [c] in [DEEP] (selected) and contains a negative
   literal [Q(t)], instance of the positive literal of [c'].
   - [c'] in [ONEVAR], [c] in [DEEP0] or [DEEP1]
   and contains deep negative literal with top predicate [Q] of [c'].
   - [c'] in [UNSEL], [c] in [DEEP] (selected) and contains a negative
   literal [Q(t)], unifiable with head literal of [c].
   - [c'] in [REG] or [UNSEL], [c] in [ONEVAR0] or [UGOAL] 
   and contains a negative literal [Q(x)] ([Q] head predicate of [c']).
   - [c'] in [ONEVAR0] or [ONEVAR2], [c] in [ONEVAR0] or [UGOAL]
   and contains a negative literal [Q(x)] ([Q] head predicate of [c']).
   - [c] in [ONEVAR2]: resolution not possible because of ordered strategy
   - [c] in [UNSEL]: resolution not possible because of ordered strategy
   - [c'] in [SPLIT]: resolution not possible because of selection strategy
   - [c'] in [DEEP]: resolution not possible because of selection strategy
   - [c'] in [UGOAL]: resolution not possible because of ordered strategy 
*)



(** [resolution_table t1 t2]
    @return true iff a resolution step is possible because a clause
    of type [t1] and a clause of type [t2]. *)
val resolution_table: 
  clausetype -> clausetype -> bool 





(** [to_string t]
      @return a string representation of the given clausetype [t]
      (4 letter string) *)
val to_string:
  clausetype -> string
