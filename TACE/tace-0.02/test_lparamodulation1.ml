
open Trace;;
open Parsing;;
open Symbol;;
open Variable;;
open Term;;
open Trs;;
open Equation;;
open Clause;;
open Lparamodulation;;


debug_on ();;
set_trace_level 5;;

let start_test () = 
  try 
    let ec = (Clause.of_string "Q(x2), Q(x1), x1 = x2 => Qf(g(x1, x2))") in
    (* empty theory *)
    let r = [] in
      begin 
	test "narrowing %s\n" (Clause.to_string ec);
	let sol = (lparamodulation r ec) in 
	  test "%i solutions\n" (List.length sol);
	  List.iter (fun c -> test "   %s\n" (Clause.to_string c)) sol
      end
  with
      (Parsing.Parse_error (s, l)) ->
	error "Parse error: %s, %s\n" s (Location.to_string l);;
