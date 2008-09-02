
open Format;;
open Trace;;
open Clause;;
open Resolution;;

set_trace_level 6;; 

let rec print_clauses i = function
    [] -> ()
  | c::l ->
      test "                   %s\n" (Clause.to_string c);
      print_clauses (i+1) l;;

let test_resolution (s1, s2) =   
  let c1 = Clause.of_string s1 in
  let c2 = Clause.of_string s2 in
    test "... c1           = %s\n" (Clause.to_string c1);
    test "... c2           = %s\n" (Clause.to_string c2);
    (* Wclause.dump ts wc;     *)
    let cl =  (resolution c1 c2) in
      test "... resolution   =\n";
      print_clauses 1 cl;
      test "*************************************************************\n";;


let start_test () = 
  debug_on ();
  List.iter test_resolution
    [ 
      (* OCL OCL *)
      ("Q1(x), Q2(x) => Q3(x)", "Q3(y) => Q4(y)");
      ("Q1(x), Q2(x) => Q3(x)", "Q3(y) => ");
      ("Q1(x), Q2(x) => Q3(x)", "Q5(y), Q3(y), Q6(y) => Q4(y)");
      (* REG OCL *)
      ("Q1(x1), Q2(x2) => Q3(f(x1, x2))", "Q3(y) => Q4(y)");
      ("Q1(x1), Q2(x2) => Q3(f(x1, x2))", "Q3(y) => ");
      ("Q1(x1), Q2(x2) => Q3(f(x1, x2))", "Q5(y), Q3(y), Q6(y) => Q4(y)");
      (* OCL DEEP *)
      ("Q1(x), Q2(x) => Q3(x)", "Q3(f(a, g(y))) => Q4(y)");
      ("Q1(x), Q2(x) => Q3(x)", "Q5(a), Q3(f(a, g(y))), Q6(y) => Q4(y)");
      (* REG DEEP *)
      ("Q1(x1), Q2(x2) => Q3(f(x1, x2))", "Q3(f(a, g(y))) => Q4(y)");
      ("Q1(x1), Q2(x2) => Q3(f(x1, x2))", "Q5(a), Q3(f(a, g(y))), Q6(y) => Q4(y)");
      (* DEEP OCL *)
      ("Q1(x), Q2(x) => Q3(f(x,x))", "Q3(y) => Q4(y)");
      ("Q1(x1), Q2(x2) => Q3(f(x1, x2))", "Q3(y) => Q4(y)");
      ("Q1(x1), Q2(x2) => Q3(f(x1, x2))", "Q3(y) =>");
      ("Q1(x), Q2(x) => Q3(f(x, x))", "Q3(y) => Q4(y)");
      ("Q1(x1), Q2(x1), Q3(x2) => Q4(h(g(x1), f(x2, a)))", "Q4(y), Q3(y), Q6(y) => Q5(y)");
      ("Q1(x), Q2(x) => Q3(f(x, x))", "Q3(y) =>");
      (* DEEP DEEP *)
      ("Q1(x), Q2(x) => Q3(g(x))", "Q3(g(y)) => Q4(y)");
      ("Q1(x1), Q2(x2) => Q3(f(x1,x2))", "Q3(f(y1,y2)) => Q4(g(y1))");
      ("Q1(x1), Q2(x2) => Q3(f(x1,x2))", "Q3(f(y1,y2)) =>");
      ("Q1(x), Q2(x) => Q3(f(x,x))", "Q3(f(y1,y2)) => Q4(y2)");
      ("Q1(x), Q2(x) => Q3(f(x,x))", "Q3(f(y1,y2)) =>");
      ("Q1(x1), Q2(x1), Q3(x2) => Q4(h(g(x1), f(x2, a)))",
       "Q6(y), Q3(y), Q4(h(g(y),y)) => Q5(y)");
      ("Q1(x1), Q2(x1), Q3(x2) => Q4(h(g(x1), f(x2, a)))",
       "Q3(y), Q4(h(y, f(b, y1))), Q6(y) => Q5(y)");
      ("Q1(x1), Q2(x1), Q3(x2) => Q4(h(g(x1), f(x2, a)))",
       "Q3(y), Q4(h(y, f(b, y1))), Q6(y) => Q5(f(y,y))");
      ("Q1(x), Q2(x), Q3(y) => Q4(h(g(x), f(y, a)))",
       "Q3(y), Q4(h(y, f(b, x))), Q6(y) => Q5(f(y,y))");
    ];;
    




    
