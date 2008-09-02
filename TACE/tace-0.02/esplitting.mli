(** Espilon-splitting *)

open Symbol
open Clause

(* TODO monitor *)

(** [esplit c]
    @return the list of {! Clause.clause}s 
    obtained by epsilon-splitting of the clause [c], 
    or the empty list if [c] is not splittable. 

    esplitting [B1, ..., Bk, L => A] returns the clause, in this order
    ([B1], ..., [Bk] are espilon blocks variable-disjoint from [L], [A]):
    - [q1, ..., qk, L => A]
    - [B1 => q1]
    - ...
    - [Bk => qk] 

    [q1],...,[qk] are nullary splitting predicates, 
    each [qi] is unically associated to [Bi]. *)
val esplit:
  clause -> clause list


(** [make_clause [q]]
    @return the clause [Q1(x),...,Qn(x) => q]
    where [q] is the nullary splitting predicate symbol 
    uniquely associated to [Q1; ...; Qn]. *)
val make_clause:
  symbol -> clause


(** [dump ()]
    print a list of the epsilon-splitting predicate symbols created,
    with the corresponding lists of predicates. *)
val dump:
  unit -> unit
