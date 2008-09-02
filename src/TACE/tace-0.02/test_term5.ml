
open Printf;;
open Trace;;
open Term;;
open Variable;;

set_trace_level 5;;

let test_termstring s = 
  printf "term %s = %s\n" s (Term.to_string (Term.of_string s));;

let start_test () = 
  let t = ref (Term.of_string "f(g(z1), f(g(x), h(z1)))") in
    begin
      printf "term: %s\n" (Term.to_string !t);
      t := block !t;
      printf "blocked: %s\n" (Term.to_string !t);
      t := unblock !t;
      printf "unblocked: %s\n" (Term.to_string !t);
      t := map_var (set_color 1) !t;
      printf "colored (1): %s\n" (Term.to_string !t);
      t := map_var decolor !t;
      printf "decolor: %s\n" (Term.to_string !t);
      t := map_var (set_color 2) !t;
      printf "colored (2): %s\n" (Term.to_string !t);
      t := block !t;
      printf "tagged & blocked: %s\n" (Term.to_string !t);  
      t := map_var decolor !t;
      printf "decolor: %s\n" (Term.to_string !t);
    end;;
      
