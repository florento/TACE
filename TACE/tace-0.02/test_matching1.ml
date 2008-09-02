
open Printf;;
open Trace;;
open Symbol;;
open Variable;;
open Term;;
open Substitution;;
open Matching;;

set_trace_level 6;;

let test_matching st1 st2 = 
  printf "read %s and %s\n" st1 st2;
  let t1 = Term.of_string st1 in 
  let t2 = Term.of_string st2 
  in printf "%s matches %s: %s\n" 
       (Term.to_string t1) (Term.to_string t2)
       (match (matcher t1 t2) with
	    None -> 
	      "   nope"
	  | Some subst -> 
	      sprintf "   with: { %s }" (Substitution.to_string subst));;

let start_test () = 
  begin
    test_matching "x"          "a";
    test_matching "b"          "y";
    test_matching "y"          "a";
    test_matching "a"          "b";
    test_matching "f(x, y)"    "f(a, b)";
    test_matching "f(x, x)"    "f(a, b)";
    test_matching "f(x, x)"    "f(a, a)";
    test_matching "f(x, y)"    "f(a, g(x))";
    test_matching "f(x, g(x))" "f(g(z), g(g(z)))";
  end;;

