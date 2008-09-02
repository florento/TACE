
open Array;;
open List;;

open Trace;;
open Chrono;;
open Util;;
open Symbol;;
open Atom;;
open Substitution;;
open Equation;;
open Clause;;
open Clausetype;;
open Matching;;
open Clauseset;;


(****************)
(** Monitoring  *)
(****************)


type monitor = {
  mutable sub_tested : int;
  mutable sub_success : int;
  mutable sub_fail_filter : int array;
  mutable sub_fail_other : int;
  mutable sub_time : chrono;
};;


let monitor =     
  {
    sub_tested = 0;
    sub_success = 0;
    sub_fail_filter = [| 0; 0; 0; 0; 0; 0 |];
    sub_fail_other = 0;
    sub_time = Chrono.make "ch_subsumption";
  };;


let incr_cpt_filter i = 
  assert (i >= 0);
  assert (i <= 5);  
  monitor.sub_fail_filter.(i) <- monitor.sub_fail_filter.(i) + 1;;


(******************************)
(**  End of subsumption test  *)
(******************************)


let fail_filter i c1 c2 = 
  assert (debug_b "subsumption.filter%i %s  %s FAILED\n"
	    i (Clause.to_stringt c1)  (Clause.to_stringt c2));
  Chrono.stop monitor.sub_time;
  incr_cpt_filter i;
  false;;


let fail_subsume c1 c2 = 
  assert (debug_b "Subsumption.subsumption FAILED  %s  %s\n" 
	    (Clause.to_stringt c1)  (Clause.to_stringt c2));
  Chrono.stop monitor.sub_time;
  monitor.sub_fail_other <- monitor.sub_fail_other + 1;
  false;;


let success_subsume c1 c2 = 
  assert (debug_b "Subsumption.subsumption SUCCESS  %s  %s\n" 
	    (Clause.to_stringt c1)  (Clause.to_stringt c2));
  Chrono.stop monitor.sub_time;
  monitor.sub_success <- monitor.sub_success + 1;
  assert (debug_b "%s subsumed by %s\n" 
	    (Clause.to_stringt c2)
	    (Clause.to_stringt c1));
  true;;


(****************)
(**   Filters   *)
(****************)


(** filter 0: 
    we do not test equations for subsumption here *)
let filter_eq c1 c2 = 
  (get_equations NEG c1 = []) && (get_equations POS c1 = []);;


(** filter 1, as defined in the documentation of SPASS:
    size

    ATTENTION: for multiset subsumption only *)

let filter_size c1 c2 = 
  (Clause.size c1) <= (Clause.size c2);;


(** filter 2:
    case of failure of subsumption detected from the respective clausetypes. *)

let filter_type c1 c2 = 
  let t1 = Clause.get_type c1 in
  let t2 = Clause.get_type c2 in    
    (* a regular clause can only be subsumed by itself *)
    if (t2 = REG && t1 <> REG)
    then false
    (* a ONEVAR clause can only be subsumed a ONEVAR or UGOAL *)
    else if (t2 = ONEVAR && t1 <> ONEVAR && t1 <> UGOAL)
    then false
    (* a positive splitting clause can only be subsumed by herself *)
    else if (t2 = SPOS && t1 <> SPOS)
    then false
    (* a clause with splitting literal can only split a clause with splitting *)
    else if (t1 = SPLIT && t2 <> SPLIT)
    then false
    (* a clause with deep literal can only split a clause with deep literal *)
    else if (t1 = DEEP && t2 <> DEEP && t2 <> SPLIT)
    then false
    else true;;


(** filter 3:
    number of literals 

    ATTENTION: for multiset subsumption only *)

let filter_lengthlit c1 c2 = 
  (Clause.lengths POS c1) <= (Clause.lengths POS c2) 
  && (Clause.lengths NEG c1) <= (Clause.lengths NEG c2);; 


let filter_length c1 c2 = 
  List.length (get_equations POS c1) <= List.length (get_equations POS c2)
  && List.length (get_splittings POS c1) <= List.length (get_splittings POS c2)
  && List.length (get_others POS c1) <= List.length (get_others POS c2)
  && List.length (get_equations NEG c1) <= List.length (get_equations NEG c2)
  && List.length (get_splittings NEG c1) <= List.length (get_splittings NEG c2)
  && List.length (get_others NEG c1) <= List.length (get_others NEG c2);;


(** filter 4:
    subset relation between predicate symbols (neg and pos) *)


let filter_literals comp_atoms l1 l2 =
  List.for_all (fun a1 -> List.exists (fun a2 -> comp_atoms a1 a2) l2) l1;;


let test_head a1 a2 = 
  Symbol.equal (Atom.get_predicate a1) (Atom.get_predicate a2);;


let filter_pred c1 c2 = 
  filter_literals test_head (get_splittings POS c1) (get_splittings POS c2)
  && filter_literals test_head (get_others POS c1) (get_others POS c2)
  && filter_literals test_head (get_splittings NEG c1) (get_splittings NEG c2)
  && filter_literals test_head (get_others NEG c1) (get_others NEG c2);;


(** filter 5,  as defined in the documentation of SPASS: 
    for every literal [a] in the first clause,
    there exists a literal of same sign matched by [a] in the second clause
    (test for existence of matchers separately, without checking compatibility). *)


let filter_match c1 c2 = 
  filter_literals Atom.matching (get_splittings POS c1) (get_splittings POS c2) 
  && filter_literals Atom.matching (get_others POS c1) (get_others POS c2) 
  && filter_literals Equation.matching (get_equations POS c1) 
    (get_equations POS c2) 
  && filter_literals Atom.matching (get_splittings NEG c1) 
    (get_splittings NEG c2)
  && filter_literals Atom.matching (get_others NEG c1) (get_others NEG c2)
  && filter_literals Equation.matching (get_equations NEG c1) 
    (get_equations NEG c2);;



(*************************)
(**   Subsumption test   *)
(*************************)

(** In this implementation, we ignore negative equations:
    we say that a clause with negative equations can not subsume 
    another clause. *)


exception Subsumption_failure;;

exception Subsumption_success;;


(** multiset subsumption
    faster than set subsumption (but not complete). *)
let rec iter_subsume ll = 
  try 
    iter_subsume1 (Substitution.make ()) ll;
    false
  with
      Subsumption_failure -> false
    | Subsumption_success -> true 
and iter_subsume1 s = function
    [] -> raise Subsumption_success
  | (l1, l2)::ll ->
      iter_subsume2 false s l1 l2 [] ll
and iter_subsume2 flag_ignore s l1 l2 back rest =   
  match (l1, l2) with
      [], _ -> 
	iter_subsume1 s rest
    | (a::_), [] -> 
	if flag_ignore
	(* one matcher was ignored, ignore this failure *)
	then ()
	(* no matching for an element of l1: failure *)
	else raise Subsumption_failure
    | (a::la), (b::lb) ->
	match (Atom.matcher a b) with
	    None -> 
	      iter_subsume2 flag_ignore s l1 lb (b::back) rest
	  | Some sab -> 
	      if compatible sab s
	      then begin
		(* next of la *)
		iter_subsume2 false (Substitution.union sab s) 
		  la (List.rev_append back lb) [] rest;
	        (* or ignore b an try another branch *)
		iter_subsume2 true s l1 lb (b::back) rest
	      end
	      else 
		iter_subsume2 flag_ignore s l1 lb (b::back) rest;;


let subsume2 c1 c2 = 
  if iter_subsume
    [
      ((get_splittings POS c1), (get_splittings POS c2));
      ((get_others POS c1), (get_others POS c2));
      ((get_splittings NEG c1), (get_splittings NEG c2));
      ((get_others NEG c1), (get_others NEG c2))
    ]
  then 
    success_subsume c1 c2
  else 
    fail_subsume c1 c2;;


let subsume c1 c2 = 
  assert (debug_b "Subsumption.subsume %s  %s\n" 
	    (Clause.to_stringt c1) (Clause.to_stringt c2));
  monitor.sub_tested <- monitor.sub_tested + 1;
  Chrono.start monitor.sub_time;
  (* we do not test equations for subsumption *)
  if not (filter_size c1 c2)
  then fail_filter 1 c1 c2
  else if not (filter_eq c1 c2)
  then fail_filter 0 c1 c2
  else if not (filter_type c1 c2)
  then fail_filter 2 c1 c2
  else if not (filter_length c1 c2)
  then fail_filter 3 c1 c2
  else if not (filter_match c1 c2)
  then fail_filter 5 c1 c2
  else subsume2 c1 c2;;

