
open Printf;;

open Trace;;
open Util;;
open Parsing;;
open Symbol;;
open Variable;;
open Term;;
open Trs;;
open Equation;;
open Narrowing;;


debug_on ();;
set_trace_level 5;;


let rec trs_of_list = function
    [] -> Trs.make ()
  | (se)::l -> Trs.add (Equation.of_string se) (trs_of_list l);;


let rec print_sols i = function
    [] -> ()
  | s::l ->
      test "sol%i: %s\n" i (Substitution.to_string s);
      print_sols (i+1) l;;


let start_test () = 
  debug_on ();
  set_trace_level 6;
  try 
    let r = trs_of_list
      [ "sd(se(z1,z2),z2) = z1";
	"ad(ae(z1,z2),inv(z2)) = z1";
	"ad(ae(z1,inv(z2)),z2) = z1";
	"inv(inv(z)) = z";
	"fst(pair(z1,z2)) = z1";
	"snd(pair(z1,z2)) = z2" ] in
    let t = Term.of_string "se(s,ad(ad(snd(y1),inv(pub(b))), pub(fst(y1))))" in
    let t2 = Term.of_string "se(s, ad(ad(z2, inv(pub(b))), pub(z1)))" in
    let y2 = Term.of_variable (Variable.of_string "y2") in
    let e = Equation.make y2 t in 
      test "TRS = %s\n" (Trs.to_string r);
      test "1 step narrowing term %s returns: %s\n"
	(Term.to_string t)
	(Util.list_to_string 
	   (fun (t1, s1) -> sprintf "\n (%s, %s)"
	      (Term.to_string t1) 
	      (Substitution.to_string s1)) 
	   (narrow r t));
	test "1 step narrowing equation %s returns: %s\n"
	  (Equation.to_string e)
	  (Util.list_to_string 
	     (fun (e1, s1) -> sprintf "\n (%s, %s)" 
		(Equation.to_string e1)
		(Substitution.to_string s1)) 
	     (narrow_equation r e));
	test "solving equation %s\n" (Equation.to_string e);
	(let sol = (solve r e) in 
	   test "%i solutions\n" (List.length sol);
	   print_sols 0 sol);
	test "1 step narrowing term %s returns: %s\n"
	  (Term.to_string t2)
	  (Util.list_to_string 
	     (fun (t1, s1) -> sprintf "\n (%s, %s)"
		(Term.to_string t1) 
		(Substitution.to_string s1)) 
	     (narrow r t2));
  with
      (Parsing.Parse_error (s, l)) ->
	printf "Parse error: %s, %s\n" s (Location.to_string l);;
