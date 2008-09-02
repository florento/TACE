(* regular *)

open Trace;;
open Clause;;

debug_on ();;
set_trace_level 6;;

let test_clause s = 
  let c = (Clause.of_string s) 
  in test "clause %s is regular: %B\n" (Clause.to_string c) (is_regular c);;

let start_test () = 
  List.iter test_clause
    [ "Q(x), Q(y) => Q(f(x, y))";
      "Q(x), Q1(y) => Q(f(x, x))";
      "Q(x), Q1(y), Q2(z) => Q1(h(x, y, z))"; 
      "Q(x), Q1(y), Q2(z) => Q1(f(x, y))";
      " => Q(a)";
      " => Q(g(x))";
      "Q(x) => Q(g(x))" ];;
    
