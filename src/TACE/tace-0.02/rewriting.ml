
open List;;

open Trace;;
open Term;;
open Equation;;
open Substitution;;
open Matching;;

(** [rewrite_root r t]
    @return the list of term obtained from [t] 
    by exactly 1 step rewriting at root position with [r].  *)
let rewrite_root r t = 
  let rec rewrite_root1 tl t = function
      [] -> tl
    | e::l -> 
	match matcher (Equation.left e) t with
	    None -> rewrite_root1 tl t l
	  | Some s -> 
	      rewrite_root1 
		((Substitution.substitute s (Equation.right e))::tl) t l
  in rewrite_root1 [] t r;;


(** [rewrite_root_first r t]
    @return [Some t'] is [t'] is obtained from from [t] 
    by exactly 1 step rewriting at root position with 
    the first matching rule in [r], returns [None] if
    there is no matching rule. *)
let rewrite_root_first r t = 
  assert (debug_b "Rewriting.rewrite_root_first %s\n" 
	    (Term.to_string t));
  let rec rewrite_root_first1 t = function
      [] -> None
    | e::l -> 
	match matcher (Equation.left e) t with
	    None -> 
	      rewrite_root_first1 t l
	  | Some s -> 
	      Some (Substitution.substitute s (Equation.right e))
  in rewrite_root_first1 t r;;


type rew_sub = 
    REW_NF of term list
  | REW_RE of term list;;


(** [rewrite_root_first r t]
    @return [Some t'] is [t'] is obtained from from [t] 
    by exactly 1 step rewriting at leftmost innermost position 
    with the first matching rule in [r], returns [None] if
    there is no matching rule. *)
let rec rewrite_leftmost_first r t = 
  assert (debug_b "Rewriting.rewrite_leftmost_first %s\n" 
	    (Term.to_string t));
  if Term.is_variable t
  then None
  else 
    let ot = (rewrite_root_first r t) 
    in match ot with
	None ->
	  (match (rewrite_leftmost_first1 r [] (subterms t)) with
	      None  -> None
	    | Some l -> 
		Some (Term.make (topsymbol t) l))
      | Some _ -> 
	  ot
and rewrite_leftmost_first1 r prev = function
    [] -> None
  | t::l -> 
      match (rewrite_leftmost_first r t) with
	  None -> 
	    (rewrite_leftmost_first1 r (t::prev) l)
	| Some t1 -> 
	    Some (rev_append prev (t1::l));;


let normal_form r t = 
  match (rewrite_leftmost_first r t) with
      None -> true
    | Some t1 -> false;;


let rec normalize r t =
  assert (debug_b "Rewriting.normalize %s\n" (Term.to_string t));
  match (rewrite_leftmost_first r t) with
      None -> t
    | Some t1 -> normalize r t1;;


let equation_normalize r e =
  Equation.make 
    (normalize r (Equation.left e)) 
    (normalize r (Equation.right e));;
