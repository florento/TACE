
open Printf;;
open Format;;

open Trace;;
open Clause;;
open Esplitting;;

let test_esplit s =   
  let c = Clause.of_string s in
  let cl = (esplit c) in 
    assert (Clause.check c);
    test "...esplit clause %s\n" (Clause.to_stringt c);
    List.iter (fun c -> 
		 assert (Clause.check c);
		 test "...   %s\n" (Clause.to_stringt c)) cl;
    test "*************************************************************\n";;


let start_test () = 
  debug_on ();
  set_trace_level 6;
  List.iter test_esplit
    [ 
      "Q1(x1), Q2(x2) => Q3(g(x1,x2))";
      "Q1(x1), Q2(x2) => Q(x2)";
      "Q1(x), Q2(x) => Q3(g(x,x))";
      "Q1(x1), Q2(x2) => ";
      "Q1(x), Q2(x) => ";
      "Q1(x), Q2(x), Q3(h(x)) => ";
      "Q3(h(y)) => Q4(y)";
      "Q1(x1), Q2(x2), Q3(x1) => Q(x2)";
      "Q1(x1), Q2(x2), Q3(x1) => Q(h(x2))";
      "Q1(x1), Q2(x2), Q3(x1), Q4(x1) => Q(h(x2))";   
      "Q1(x1), Q2(x2), Q3(x3) => Q3(f(x1,x2))";
      "Q3(f(y1,y2)), Q3(f(y2,y1)) =>";
      "Q3(f(y1,y2)), Q3(f(y1,y2)) =>";
      "Q3(f(y1,y2)) => Q4(h(y1))";
      "Q(x), Q(x) => Q3(h(x))";
      "Q1(x1), Q0(x2) => Q1(x2)"
    ];;    




    
