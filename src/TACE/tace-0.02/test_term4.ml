
open Printf;;
open Trace;;
open Term;;

set_trace_level 6;;


let test_termstring s = 
  printf "term %s = %s\n" s (Term.to_string (Term.of_string s));;

let start_test () = 
  List.iter test_termstring
    [ "f(g(g(g(1))), f(g(x), g(f(a,a, h(a,a,a,a))), g(f(a, x, g(b)))), 
       h(1, 2, 3, p(f(a, g(a), b), y)))"; 
      "h(a, )" ];;
    
