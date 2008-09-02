
open List;;

open Trace;;
open Util;;
open Equation;;
open Clause;;
open Clausetype;;

let elim c = 
  assert (debug_b "Elim.elim %s\n" (Clause.to_stringt c));    
  if (get_type c = EQ)
  then 
    let non_trivial = (fun e -> (not (Equation.trivial e))) 
    in Clause.filter (fun a -> true) non_trivial c
  else c;;

