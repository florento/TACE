(** Saturated sets of clauses with atoms stored in a {! Termset} *)

open Symbol
open Variable
open Term
open Atom
open Trs
open Clause
open Clausetype
open Instruction

(** {2 Types} *)

(** It is an abstract type containing:
    - a list of {! Clause.clause}s,
    saturated under basic ordered paramodulation with selection and splitting.  
    - a rewrite system {! Trs.trs} 
    
    Each clause is stored along with the intruction that created it.

    Deletion: when a clause is deleted from a clauseset with 
    {! Clauseset.delete} or {! Clauseset.delete_all},
    its index is not taken by another clause (with {! Clauseset.add}), 
    and the corresponding instruction
    remains accessible with {! Clauseset.get_instruction}. *)  
type clauseset


(**
   A [clauseset] can be used as an environment 
   for the execution of {! Instruction}s, see module {! Interpreter}.
   The execution of an instruction in a clauseset returns clauses, 
   the addition of these to the clauseset with {!Clauseset.add} 
   returns new instructions for saturation.
   In this approach, only the instructions need to be stored 
   (the list of pending clauses is useless) but each saturation step
   is performed twice (once in {!Clauseset.add} to evaluate the pending
   instruction list, once for the execution of these instructions).

   If the pending clauses are stored, 
   the list of pending instructions is useless,
   and each saturation step is performed only once 
   (but this is memory consuming). *)




(** {2 Creation} *)


(** [make ts r]
    @return a new [clauseset] whose trs is [r],
    and set of clauses is empty. *)
val make:
  trs -> clauseset


(** [makenew ()]
    @return a new [clauseset] whose trs is empty. *)
val makenew:
  unit -> clauseset


(** [set_trs r cs]
    set the trs of [cs] to [r]. *)
val set_trs:
  trs -> clauseset -> unit



(** {2 Access} *)


(** [length cs]
    @return the number of clauses in the clauseset [cs]. *)
val length:
  clauseset -> int


(** [size cs]
    @return the global size of the clauseset [cs],
    sum of the respective sizes of the clauses contained. *)
val size:
  clauseset -> int


(** [deleted cs]
    @return the number of clauses which have been deleted
    in the clauseset [cs]. *)
val deleted:
  clauseset -> int


(** [get_trs cs]
    @return the trs of the given [clauseset] [cs]. *)
val get_trs:
  clauseset -> trs


(** [mem i cs]
    @return true iff there is a clause of index [i]
    (deleted or not) in the clauseset [cs]. *)
val mem:
  int -> clauseset -> bool


(** [get_clause cs i]
    @return the clause of index [i] in the clauseset [cs].
    The index of a clause is returned at insertion with {! Clauseset.add}. 
    @raise Not_found if there is no clause of index [i] in [cs]. *)
val get_clause:
   int -> clauseset -> clause


(** [is_deleted cs i]
    @return true iff the clause at index [i] in the clauseset [cs]
    has been deleted.
    The index of a clause is returned at insertion with {! Clauseset.add}. 
    @raise Not_found if there is no clause of index [i] in [cs]. *)
val is_deleted:
   int -> clauseset -> bool


(** [get_clause2 cs i]
    @return a pair with 
    - the clause of index [i] in the clauseset [cs]
    - a flag whether the clause at index [i] in [cs] has been deleted.

    The index of a clause is returned at insertion with {! Clauseset.add}. 
    @raise Not_found if there is no clause of index [i] in [cs]. *)
val get_clause2:
  int -> clauseset -> (clause * bool)


(** [get_instruction cs i]
    @return the instruction that produced the clause of index [i] 
    in the clauseset [cs].
    @raise Not_found if there is no clause of index [i] in [cs]. *)
val get_instruction:
   int -> clauseset -> instruction


(** [get i cs]
    @return a pair made of the clause of index [i] in the clauseset [cs]
    along with the instruction that produced it.
    The index of a clause is returned at insertion with {! Clauseset.add}. 
    @raise Not_found if there is no clause of index [i] in [cs]. *)
val get:
  int -> clauseset -> clause * instruction


(** [exists p cs]
    @return true iff at least one non deleted clause of the clauseset [cs] 
    satisfies the predicate [p]. *)
val exists:
  (clause -> bool) -> clauseset -> bool



(** {2 List operations} *)


(** [map f cs]
    @return the list of all the elements obtained 
    by application of [f] to the clauses of [cs]. *)
val map:
  (clause -> 'a) -> clauseset -> 'a list


(** [mapi f cs]
    @return the the list of all elements 
    obtained by application of [f] to the pairs made of 
    a clause of [cs] together with its index in [cs]. *)
val mapi:
  (clause -> int -> 'a) -> clauseset -> 'a list


(** [flatmap f cs]
    @return the concatenation of all the lists 
    of elements obtained by application of [f] to the clauses of [cs]. *)
val flatmap:
  (clause -> 'a list) -> clauseset -> 'a list


(** [flatmapi f cs]
    @return the concatenation of all the lists of elements 
    obtained by application of [f] to the pairs made of 
    every clause of [cs] together with it index in [cs]. *)
val flatmapi:
  (clause -> int -> 'a list) -> clauseset -> 'a list




(** {2 Modification} *)


(** [delete_all p cs]
    delete all the clauses of [cs] which satisfy the predicate [p]. *)
val delete_all:
  (clause -> bool) -> clauseset -> unit


(** [add c i cs]
    @return the position ([>= 0]) at which the clause [c] 
    has been added in [cs],
    along with the instruction [i] that produced it.
    If [c] is an equation (equational positive clause)
    then [c] is added to the rewrite system of [cs] (at position returned),
    otherwise, it is added in the list of clauses of [cs]. *)
val add:
  clause -> instruction -> clauseset -> int


(** {2 Backward and Forward Subsumption} *)


(** [forward_subsume cs c]
   @return [true] iff one clause of the clauseset [cs]
   subsumes the clause [c]. *)
val forward_subsume:
  clauseset -> clause -> bool


(** [backward_subsume c cs]
    remove the clauses from the clauseset [cs] subsumed by the clause [c]. *)
val backward_subsume:
  clause -> clauseset -> unit




(** {2 Output} *)

(** [dump cs]
    print the contents of the clauseset [cs]. *)
val dump:
  clauseset -> unit
