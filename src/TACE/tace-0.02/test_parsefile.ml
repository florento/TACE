
open Printf;;
open Format;;

open Trace;;
open Util;;
open Resolution;;
open Parsefile;;

let start_test () = 
  debug_on ();
  set_trace_level 6;
  let (r, cl) = to_clauses "../examples/ta05.txt" in
    test "...CLAUSES parsed\n";
    List.iter (fun c -> test "...%s\n" (Clause.to_stringt c)) cl;
    test "...REWRITE RULES parsed\n";
    List.iter (fun e -> test "...%s\n" (Equation.to_string e)) r;
    




    
