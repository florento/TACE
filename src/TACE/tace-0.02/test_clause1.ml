
open Trace;;
open Parsing;;
open Clausetype;;
open Clause;;


let test_clause s = 
  try
    let c = (Clause.of_string s) in
      test "...parse: %s\n" s;
      test "...clause: %s\n" (Clause.to_string c);
      test "...type: %s\n" (Clausetype.to_string (Clause.get_type c));
      test "...size: %i\n" (Clause.size c);     
      test "...is horn? %s\n" (string_of_bool (Clause.is_horn c));
      test "...is positive? %s\n" (string_of_bool (Clause.is_positive c));
      test "...is goal? %s\n" (string_of_bool (Clause.is_goal c));
  with
      Parse_error (msg, loc) ->
	error "%s: %s\n" (Location.to_string loc) msg;
	failwith "Parse error"


let start_test () = 
  debug_on ();
  set_trace_level 6;
  List.iter test_clause
    [ "Q1(x), Q2(x) =>";
      "Q11(x), Q12(y) => Q13(f(x, y))";
      "Q1(x), Q2(y) => Q2(h(y, x))";
      "Q1(x) => Q2(g(x))";
      "Q1(x2), Q2(x3), Q3(x1) => Q(m(x1,x2,x3))";
      "Q21(x), Q22(g(y)) => Q23(f(x, y))";      
      "Q31(x), Q32(h(y, x)) => Q33(x)";
      "Q41(x), Q42(x) => Q(x)";
      "Q1(x), Q2(x) => Q(f(x,x))";
      "Q1(x), Q2(x) =>";
      "Q1(x), Q2(y), x = y => Q3(f(x, y))";
      "Q1(x), g(x) = y, Q2(y) => Q3(f(x, y))";
      "y = x, Q1(x), Q2(y) => Q3(f(x, y))";
      "Q1(x), Q2(g(y)), x = y => Q3(f(x, y))";
      "Q1(x), Q2(h(y, x)) => Q3(x)";
      "Q1(x), x = x2, Q2(x2) =>"
    ];;
