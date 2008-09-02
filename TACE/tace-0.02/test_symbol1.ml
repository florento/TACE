
open Printf;;
open Trace;;
open Symbol;;

set_trace_level 6;;


let test_make_symbol name arity kind = 
  let n = (make name arity kind) in ();;
(*  in printf "make_symbol %s\n" (Symbol.to_string n);; *)

let start_test () = 
  (test_make_symbol "a" 0 FUNCTION);
  (test_make_symbol "f" 2 FUNCTION);
  (test_make_symbol "g" 1 FUNCTION);
  (test_make_symbol "P0" 1 PREDICATE);
  (test_make_symbol "P1" 1 PREDICATE);
  (test_make_symbol "P2" 1 PREDICATE);
  (test_make_symbol "q1" 0 SPLITTING);
  (test_make_symbol "q2" 0 SPLITTING);
  (Symbol.dump_symbols ());;  

