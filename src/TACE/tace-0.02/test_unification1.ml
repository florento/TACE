
open Printf;;
open Trace;;
open Symbol;;
open Variable;;
open Term;;
open Substitution;;
open Unification;;

set_trace_level 6;;

let test_unify (st1, st2) = 
  let t1 = (Term.of_string st1) in
  let t2 = (Term.of_string st2) in
    test "unify %s %s:\n" (Term.to_string t1) (Term.to_string t2);
    match (unify t1 t2) with
	None -> 
	  test ".....   no solution\n"
      | Some subst -> 
	  test ".....   solution: %s\n" (Substitution.to_string subst);;


let start_test () = 
  List.iter test_unify 
    [
      ("x", "a");
      ("b", "y");
      ("y", "a");
      ("a", "b");
      ("f(x, x)", "f(a, b)");
      ("f(x, x)", "f(a, a)");
      ("h(y, f(b, x))", "h(g(x1), f(y1, a))");
      ("ad(z2, inv(pub(b)))", "ad(ae(z1,z2),inv(z2))");
    ];;
