
(***********************************************)
(*   blocking of subterms for basic strategies *)
(***********************************************)


(** We do not use abstract data type for equations and TRSs
    (no use of build and access functions of these modules) *)

open List;;  

open Trace;;
open Util;;
open Symbol;;
open Variable;;
open Term;;
open Equation;;
open Trs;;
open Substitution;;
open Unification;;
open Matching;;
open Rewriting;;


let block_substitution = 
  Substitution.map Term.block;;


let unblock_substitution = 
  Substitution.map Term.unblock;;


let var_decolor_substitution = 
  Substitution.map_var Variable.decolor;;


(* TODO politique de coloriage des variables dans les clauses *)
let rec narrow r t = 
  assert (debug_b "Narrowing.narrow %s\n" (Term.to_string t)); 
  assert (Trs.check r);
  narrow1 r t
and narrow1 r = function
    V _ -> ([] : (term * substitution) list)
  | T (f, tl) as t -> rev_append (hnarrow r t) (multi r f [] [] tl)
and hnarrow1 e t =
  if is_blocked t
  then None
  else 
    (match (unify t (Equation.left e)) with
	 None -> None
       | Some subst -> 
	   let bsubst =
	     block_substitution (var_decolor_substitution subst) in
	   let bt2 = Term.block (Equation.right e) 
	   in Some ((Substitution.substitute bsubst bt2), bsubst))
and hnarrow r t = 
  deoptionize (Trs.map (fun e -> hnarrow1 e t) r)
  (* fold_left (fun a p -> rev_append a (hnarrow1 p t)) [] r *)
and multi r f first l0 = function
    [] -> first
  | t::l -> 
      let tl = narrow r t in 
      let tl1 = List.map (fun (x, s) -> (T (f, (rev_append l0 (x::l))), s)) tl 
      in multi r f (rev_append tl1 first) (t::l0) l;;
  
(* TBD normalize term and subtitutions returned *)


let narrow_equation r e = 
  assert (debug_b "Narrowing.narrow_equation %s\n" (Equation.to_string e));
  let e1 = Equation.left e in
  let e2 = Equation.right e 
  in (List.map 
	(fun (x, subst) -> 
	   (Equation.make x (Substitution.substitute subst e2), subst))
	(narrow r e1)) @
     (List.map 
        (fun (x, subst) -> 
	   (Equation.make (Substitution.substitute subst e1) x, subst))
        (narrow r e2));;

      
let solve r e = 
  assert (debug_b "Narrowing.solve %s\n" (Equation.to_string e));
  let rec solve1 sols  = function
      [] -> sols
    | (equ, subst)::l -> 
	let s = Equation.left equ in
	let t = Equation.right equ in
	(* try syntactic unification *)
	let sols1 = 	  
	  (match (unify s t) with
	       None -> sols	       
	     | Some sol -> 
		 Util.clean_ins 
		   (Substitution.equal) 
		   (Substitution.map (normalize r) 
		      (Substitution.compose sol subst))
		   sols) in
	(* 1 step of narrowing 
	   Note that it is not narrowable, then it is simply removed *)
	let el1 =  
	  List.map 
	    (fun (e1, subst1) -> 
	       (e1, (Substitution.compose subst subst1)))
	    (narrow_equation r equ)
	in solve1 sols1 (List.rev_append el1 l)
  in solve1 [] [ (e, (Substitution.make ()))];;
(*  in List.map 
       unblock_substitution 
       (solve1 [] [ (e, (Substitution.make ()))]);; *)

