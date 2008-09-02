
open Printf;;
open Trace;;
open Term;;

set_trace_level 6;;


let test_termstring s = 
  printf "term %s = %s\n" s (Term.to_string (Term.of_string s));;

let start_test () = 
  List.iter test_termstring
    [ "x" ; "g(x)"; "a";  "0"; "f(a, b)"; "f(h(x) , f(a , g(g( 0 ))))";
      "f(a(,b)" ];;
    
