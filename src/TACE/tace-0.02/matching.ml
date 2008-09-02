(** Module for term matching *)

open List;;

open Trace;;
open Util;;
open Symbol;;
open Variable;;
open Substitution;;
open Term;;
open Substitution;;


exception Local_matchfailure;;


(* fail immediately when non linearities not satisfied *)
let matcher s t = 
  debug "Matching.matcher %s %s\n" (Term.to_string s) (Term.to_string t);
  (* global *)
  let match_sol = ref (Substitution.make ()) in
  let rec matcher1 = function
      (V x, t) -> 
	if (Substitution.mem x !match_sol)
	then 
	  if Term.equal (Substitution.assoc x !match_sol) t
	  then ()
	  else raise Local_matchfailure
	else match_sol := Substitution.add x t !match_sol
    | (T _, V _) -> raise Local_matchfailure
    | (T (f, sl), T (g, tl)) ->
	assert (get_arity f = List.length sl);
	assert (get_arity g = List.length tl);
	if (Symbol.equal f g)
	then List.iter matcher1 (List.combine sl tl)
	else raise Local_matchfailure
  in try 
      matcher1 (s, t);
      Some !match_sol
    with 
	Local_matchfailure -> None;;


(* TBC: optimise *)
let matching t1 t2 = 
  match (matcher t1 t2) with
      Some _ -> true
    | None -> false;;





(***********************************)
(* fast versions:                  *)  
(* test aginst linearized versions *)
(***********************************)

(* see trash/fastmatching.ml  if needed *)

