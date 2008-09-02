
open Printf;;
open Trace;;
open Parsing;;
open Clause;;
open Subsumption;;


debug_on ();;
set_trace_level 5;;

let rec test_subsumption sc1 sc2 = 
  test "...read %s and %s\n" sc1 sc2;  
  let c1 = Clause.of_string sc1 in
  let c2 = Clause.of_string sc2
  in test "...%s subsumes %s : %s\n" 
       (Clause.to_string c1) (Clause.to_string c2)
       (if subsume c1 c2 then "YES SIR!" else "NOPE");;


let start_test () = 
  try 
    List.iter (fun (c1, c2) -> test_subsumption c1 c2)
      [
	(" => ", " => "); 
	("P(x1) => Q(g(x1))", " => ");
	("P(x1) => Q(g(x1))", "P(x1) => Q(g(x1))");
	("P(x1) => Q(g(x1))", "P(x1), Q2(x2) => Q(g(x1))");
	("P(x1) => Q(g(x1))", "P(x1), Q2(x2) => Q(f(x1, x2))");
	("P1(x1), P2(x2) => Q(f(x1, x2))", 
	 "P1(0), P3(b), P2(g(x2)) => Q(f(0, g(x2)))");
	("P1(x1), P2(x2) => Q(f(x1, x2))", 
	 "P1(0), P3(b), P2(g(x2)) => Q(f(0, g(g(x2))))");
	("P1(x), P2(x) => Q(g(x))", 
	 "P1(0), P3(b), P2(g(x2)) => Q(g(0))")
      ]
  with
      (Parsing.Parse_error (s, l)) ->
	printf "Parse error: %s, %s\n" s (Location.to_string l);;
