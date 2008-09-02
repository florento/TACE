(** Module for equations *)

open Printf;;
open Trace;;
open Parsing;;
open Term;;
open Substitution;;
open Rewriting;;


type equation = 
    term * term

let make t1 t2 = 
  (t1, t2);;


let rec parse_equation st = 
  (* location starts with this equation *)
  restart_location ();
  (* determine whether the first id is a variable or a symbol *)
  let s = (parse_term st) in 
    assert (debug_b "lhs = %s\n" (Term.to_string s));
    (* assert (debug_b "next = '%c'\n" (Stream.next st)); *)
    parse_equal st;
    let t = (parse_term st) 
    in (s,t)
and parse_equal st = 
  match (Stream.peek st) with
      None -> raise (parse_error "'=' expected")
    |  Some c -> 
	 (match c with 
	      '=' -> (skip st); (parse_space st)
	    | ' '
	    | '\t'
	    | '\n' -> skip st; parse_equal st
	    | _ -> raise (parse_error "'=' expected"))
and parse_space st = 
  match (Stream.peek st) with
      None -> raise (parse_error "term (rhs) expected")
    |  Some c -> 
	 (match c with 
	    | ' '
	    | '\t'
	    | '\n' -> skip st; parse_space st
	    | '=' -> raise (parse_error "term (rhs) expected")
	    | _ -> ());;



let of_string s = 
  let st = Stream.of_string s in
    parse_equation st;;
  

let to_string (s, t) = 
  sprintf "%s = %s" (Term.to_string s) (Term.to_string t);;


let left = 
  fst;;


let right = 
  snd;;


let map_var f (l, r) =
  (Term.map_var f l), (Term.map_var f r);;


let exists_var f (l, r) =
  (Term.exists_var f l) || (Term.exists_var f r);;


let for_all_var f (l, r) =
  (Term.for_all_var f l) && (Term.for_all_var f r);;


let rename ren (s, t) = 
  (Term.rename ren s, Term.rename ren t);;


let refresh e = 
  let ren = Renaming.make () 
  in rename ren e;;


let substitute s (t1, t2) =
  ((Substitution.substitute s t1), 
   (Substitution.substitute s t2));;


let matcher e1 e2 = 
  match (Matching.matcher (fst e1) (fst e2)) with
      Some s1 ->
	(match (Matching.matcher (snd e1) (snd e2)) with	     
	     Some s2 -> 
	       if Substitution.compatible s1 s2 
	       then Some (Substitution.union s1 s2)
	       else None
	   | None -> None)
    | None -> None;;


let matching e1 e2 = 
  (Matching.matching (fst e1) (fst e2)) 
  && (Matching.matching (snd e1) (snd e2));;


let equal (s1, t1) (s2, t2) = 
  ((Term.equal s1 s2) && (Term.equal t1 t2));;
  (* we do NOT assume commutativity of equality here, 
     since equations are assumed oriented [AV] *)
  (* || ((Term.equal s1 t2) && (Term.equal t1 s2));; *)


let occur x (s,t) =
  (Term.occur x s) || (Term.occur x s);;
  

let trivial (s, t) = 
  (Term.equal s t);;


let size (s, t) = 
  (Term.size s) + (Term.size t) + 1;;


let rec check (s, t) = 
  (Trace.check (Term.check s) "Equation.check: left member\n")
  && (Trace.check (Term.check t)  "Equation.check: right member\n");;
