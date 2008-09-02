
open Printf;;

open Trace;;
open Parsing;;
open Symbol;;
open Variable;;
open Term;;
open Trs;;
open Equation;;
open Narrowing;;


debug_on ();;
set_trace_level 5;;

let rec print_sols i = function
    [] -> ()
  | s::l ->
      printf "sol%i: %s\n" i (Substitution.to_string s);
      print_sols (i+1) l;;

let start_test () = 
  try 
    let l1 = (Term.of_string "fst(p(x1, x2))") in    
    let e1 = (Equation.of_string "fst(p(x1, x2)) = x1") in
    let e2 = (Equation.of_string "snd(p(x1, x2)) = x2") in
    let r = [e1; e2] in 
    let e = (Equation.of_string "fst(y) = snd(y)") in
      begin 
	printf "solving equation %s\n" (Equation.to_string e);
	printf "modulo %s\n" (Trs.to_string r);
	let sol = (solve r e) in 
	  printf "%i solutions\n" (List.length sol);
	  print_sols 0 sol
      end
  with
      (Parsing.Parse_error (s, l)) ->
	printf "Parse error: %s, %s\n" s (Location.to_string l);;
