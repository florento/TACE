(** instructions returning uclauses. *)

open Symbol
open Clause


(** Every instruction returns a list of clauses.
    It is accompagned by an index in the list returned,
    which can be used to specify a clause in the list. *)
type instruction = 
    NIL
      (** [NIL]
	  return the empty list *)
  | LPA of int * int
      (** [LPA (i, j)]
	  return the [j]th clause in the list of clauses obtained by 
	  solving one selected equation by narrowing
	  and application of eager inferences
	  to the clause of index [i] in the current clauseset. *)
  | SOL of int * int
      (** [SOL (i, j)]
	  return the [j]th clause in the list of clauses obtained by 
	  solving (by narrowing) all the negative equations 
	  and application of eager inferences
	  in the clause of index [i] in the current clauseset. *)
  | RPA of int * int
      (** [RPA (i, j)]
	  return the [j]th clause in the list of clauses obtained by 
	  right paramodulation in the head 
	  of the clause of index [i] in the current clauseset
	  and application of eager inferences.

	  It is the clause obtained by right paramodulation in the head 
	  with the [j]th rewrite rule in the head of the clause of index [i]. *)
  | NEW of clause
      (** [NEW c]
	  return clause [c]. 
	  [c] is a clause to be added to the clauseset environment. *)
  | RES of int * int * int
      (** [RES (i1, i2, j)
	  return the [j]th clause in the list of clauses obtained by 
	  resolution of 
	  of the clause of index [i1] in the current clauseset,
	  into the clause of index [i2] in the current clauseset,
	  and application of eager inferences. *)
  | ESP of symbol
      (** [ESP q
	  return the clause the clause [Q1(x),...,Qn(x) => q]
	  unically associated splitting predicate [q]
	  in the epsilon splitting procedure.
	  @see {!Esplitting.esplit}. *)


val equal:
  instruction -> instruction -> bool


(** [to_string i]
    @return a string representation of the instruction [i]. *)
val to_string:
  instruction -> string
