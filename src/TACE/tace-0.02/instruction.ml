
open Printf;;

open Util;;
open Symbol;;
open Clause;;
open Subsumption;;

type instruction = 
    NIL
  | LPA of int * int
  | SOL of int * int
  | RPA of int * int
  | NEW of clause
  | RES of int * int * int
  | ESP of symbol
;;


(** TODO refine definition for clauses and symbols ? *)
let equal i1 i2 = 
  i1 = i2;;
(*  match (i1, i2) with
      NIL, NIL -> true
    | (LPA (i1, j1)), (LPA (i2, j2)) -> i1 = i2 && i2 = j2
    | (SOL (i1, j1)), (SOL (i2, j2)) -> i1 = i2 && i2 = j2
    | (RPA (i1, j1)), (RPA (i2, j2)) -> i1 = i2 && i2 = j2
    | (NEW c1), (NEW c2) -> (subsume c1 c2) && (subsume c2 c1)
    | (RES (i11, i12, j1)), (RES (i21, i22, j2)) -> 
	i11 = i21 && i12 = i22 && j1 = j2
    | (ESP s1), (ESP s2) -> Symbol.equal s1 s2
    | _, _ -> false;;
*)
	

let to_string = function
    NIL -> "NIL"
  | LPA (i, j) -> sprintf "LPA %i (%i)" i j
  | SOL (i, j) -> sprintf "SOL %i (%i)" i j
  | RPA (i, j) -> sprintf "RPA %i (%i)" i j
  | NEW c -> sprintf "NEW %s" (Clause.to_string c)
  | RES (i1, i2, j) -> sprintf "RES %i,%i (%i)" i1 i2 j
  | ESP q ->  sprintf "ESP %s" (Symbol.to_string q);;

