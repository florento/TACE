(** Module for term unification procedures *)

open List;;

open Trace;;
open Symbol;;
open Variable;;
open Term;;
open Atom;;
open Substitution;;


exception Unification_clash;;


(** [instanciate_var subst t]
    {b internal}
    @return if the given term [t] is a variable, 
    the value associated to [t] in the substitution [subst],
    otherwise, [t] itself. *)
let instanciate_var subst = function
    (V x) as t -> Substitution.apply subst x
  | T (_, _ ) as t -> t;;
  

let unify s t =
  assert (debug_b "Unification.unify %s %s\n"
	    (Term.to_string s)
	    (Term.to_string t));
  (* return a solution or raise Unification_clash *)
  let rec unify1 sol (s0, t0) = 
    (* assert (debug_b "Unification.unify1 %s %s [%s]\n"
	      (Term.to_string s0)
	      (Term.to_string t0)
       (Substitution.to_string sol)); *)
    (* variable elimination *)
    let s1 = instanciate_var sol s0 in
    let t1 = instanciate_var sol t0 
    in match (s1, t1) with
        (* trivial *)
	V xs, V xt when xs = xt -> 
	  Substitution.make ()
          (* orient TBD: avoid rec. call *) 
      | T _, V _ -> 
	  unify1 sol (t1, s1)
      | T (f, sl), T (g, tl) -> 
	  assert (get_arity f = List.length sl);
	  assert (get_arity g = List.length tl);
	  if Symbol.equal f g
          (* decompose *)
	  then begin
	    assert (List.length sl = List.length tl);
	    fold_left unify1 sol (combine sl tl)
	  end
          (* clash *)
	  else raise Unification_clash
      | V xs, T _ -> 
	  if Term.occur xs t1
          (* clash *)
	  then raise Unification_clash
          (* variable elimination *)         
	  else 
	    let sol1 = Substitution.add xs t1 (Substitution.make ())
	    in disjoint_compose sol sol1
      | V xs, V xt -> (* here xs <> xt, this is a special case of the above *)
	  let sol1 =   Substitution.add xs t1 (Substitution.make ())
	  in disjoint_compose sol sol1
  in 
    try 
      Some (unify1 (Substitution.make ()) (s, t))	
    with 
	Unification_clash -> None;;


