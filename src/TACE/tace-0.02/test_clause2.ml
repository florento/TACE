(* refresh *)

open Trace;;
open Clause;;

set_trace_level 6;;

let test_clause s = 
  let c = (Clause.of_string s) in
  let c1 = (Clause.refresh c) 
  in test "clause %s, refresh %s\n" (Clause.to_string c) (Clause.to_string c1);;

let start_test () = 
  List.iter test_clause
    [ "Q1(x), Q2(y) => Q(f(x, y))";
      "Q1(x), Q2(y) => Q(f(x, x))";
      "Q1(x), Q2(x) => Q(f(x, x))"  ];;
    
