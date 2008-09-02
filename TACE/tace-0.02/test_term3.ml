

open Printf;;
open Trace;;
open Term;;

set_trace_level 6;;


let test_termstring s = 
  printf "term %s = %s\n" s (Term.to_string (Term.of_string s));;

let start_test () = 
  List.iter test_termstring
    ["f(x, y)"; "f(a, g(x))"; "f(g(x), g(g(a)))";  "f(g(0), g(g(1))" ];;
    
