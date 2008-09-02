
open Printf;;
open Trace;;
open Term;;

trace_off ();;

debug_off ();;

let test_vars s = 
  printf ".term: %s, vars: " s;
  let t = Term.of_string s in
  let vl = Term.vars t in 
    List.iter (fun x -> printf "%s " (Variable.to_string x)) vl;
    printf "\n\n";;
  

let start_test () = 
  test_vars "f(x, y)";
  test_vars "f(g(z1), f(g(x), h(z1)))";
  test_vars "f(g(z), f(p(x, h(p(z,y))), h(z)))";;
      
