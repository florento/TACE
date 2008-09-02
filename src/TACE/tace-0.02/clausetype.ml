
open Printf;;

open Trace;;


type clausetype = 
    UNDEF
  | EQ
  | SPOS
  | REG
  | SPLIT
  | DEEP
  | ONEVAR
  | UGOAL
  | UNSEL;;
 

let to_int = function
    UNDEF -> 0
  | EQ -> 1
  | SPOS -> 2
  | REG -> 3
  | SPLIT -> 4
  | DEEP -> 5
  | ONEVAR -> 6
  | UGOAL -> 7
  | UNSEL -> 8



let resolution_array = [|
(*             UNDEF EQ    SPOS  REG   SPLIT DEEP  OVAR  UGOAL UNSEL *)
(* UNDEF *) [| 0;    0;    0;    0;    0;    0;    0;    0;    0   |];  
(* EQ    *) [| 0;    0;    0;    0;    0;    0;    0;    0;    0   |];  
(* SPOS  *) [| 0;    0;    0;    0;    1;    0;    0;    0;    0   |];  
(* REG   *) [| 0;    0;    0;    0;    0;    1;    1;    1;    0   |];  
(* SPLIT *) [| 0;    0;    0;    0;    0;    0;    0;    0;    0   |];  
(* DEEP  *) [| 0;    0;    0;    0;    0;    0;    0;    0;    0   |];  
(* OVAR  *) [| 0;    0;    0;    0;    0;    0;    1;    1;    0   |];  
(* UGOAL *) [| 0;    0;    0;    0;    0;    0;    0;    0;    0   |];  
(* UNSEL *) [| 0;    0;    0;    0;    0;    1;    1;    1;    0   |]
|];;


let resolution_table t1 t2 =
  if resolution_array.(to_int t1).(to_int t2) = 0
  then false
  else true;;


let to_string = function
    UNDEF  -> "???"
  | EQ     -> "EQU"
  | SPOS   -> "SPS"
  | REG    -> "REG"
  | SPLIT  -> "SPT"
  | DEEP   -> "DEP"
  | ONEVAR -> "OVR"
  | UGOAL  -> "UGL"
  | UNSEL  -> "USL";;
    
