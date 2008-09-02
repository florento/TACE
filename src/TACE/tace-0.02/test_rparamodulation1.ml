
open Printf;;
open Format;;

open Trace;;
open Parsing;;
open Clausetype;;
open Clause;;
open Rparamodulation;;

let test_clause r s = 
  try
    test "\n";
    test "...parse: %s\n" s;
    let c = (Clause.of_string s) in
      test "...clause: %s\n" (Clause.to_string c);
      test "...type: %s\n" (Clausetype.to_string (Clause.get_type c));
      test "...size: %i\n" (Clause.size c);     
      test "...is regular? %s\n" (string_of_bool (Clause.is_regular c));
      let cl = rparamodulation r c in
	test "...rparamodulation:\n";
	List.iter (fun c -> test "   %s\n" (Clause.to_string c)) cl;
  with
      Parse_error (msg, loc) ->
	error "%s: %s\n" (Location.to_string loc) msg;
	failwith "Parse error"


let start_test () = 
  test "parsing TRS...\n";
  let r = 
    [
      (Equation.of_string "f(h(x1), h(x2)) = x1");
      (Equation.of_string "f(f(x1,h(h(x1))), h(x2)) = x1");
      (Equation.of_string "h(h(x)) = x");
    ] in
    test "starting tests...\n";    
    List.iter (test_clause r)
      [ 
	"Q1(x1), Q2(x2) => Q3(f(x1, x2))";
	"Q1(x1), Q2(x2) => Q3(g(x1, x2))";
	"Q1(x) => Q3(h(x))";
	"Q1(x), Q2(x) => Q(x)";
	"Q1(x), Q2(x) => Q(f(x,x))";
      ];;
