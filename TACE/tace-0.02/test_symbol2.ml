
open Printf;;
open Trace;;
open Symbol;;

set_trace_level 6;;


let test_register_symbol name arity kind = 
  let n = (make name arity kind) in ();;
(*  in printf "register_symbol %s\n" (Symbol.to_string n);; *)

let start_test () = 
  let na = (make "a" 0 FUNCTION) in
  let nP0 = (make "P0" 1 PREDICATE) in
  let nf = (make "f" 2 FUNCTION) in
  let nq1 = set_flag 1 (make "q1" 0 SPLITTING) in
  let ng = (make "g" 1 FUNCTION) in
  let nP1 = (make "P1" 1 PREDICATE) in
  let nq2 = (make "q2" 2 SPLITTING) in
  let nP2 = (make "P2" 1 PREDICATE) in
    (Symbol.dump_symbols ());;  

