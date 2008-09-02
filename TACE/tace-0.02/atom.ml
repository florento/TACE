
open List;;
open Printf;;

open Trace;;
open Parsing;;
open Util;;
open Symbol;;
open Term;;
open Substitution;;
open Rewriting;;


type atom = { 
  at_atom : term;
  (** the atom as a term headed by predicate symbol *)
  at_size : int;
  (** size of the term *)
  at_selected : bool
  (** flag whether the atom is selected *)  
};;



(*****************************)
(* Output                    *)
(*****************************)


let is_selected a = 
  a.at_selected;;


let to_term a = 
  a.at_atom;;


let to_string a =
  (if (is_selected a) then "*" else "")^(Term.to_string (to_term a));;



(*****************************)
(* Construction              *)
(*****************************)


let of_term t =
  assert (is_atom t); 
  { at_atom = t; 
    at_size = (Term.size t); 
    at_selected = false };;


let make s tl = 
  try
    if (get_kind s <> PREDICATE) && (get_kind s <> SPLITTING)
    then begin
      error "Atom.make: top symbol %s has kind %s\n" 
	(Symbol.to_string s) (Symbol.kind_to_string (get_kind s));
      failwith "Atom.make"
    end
    else if (Symbol.get_arity s) <> (List.length tl)
    then begin
      error "Atom.make: unexpected number of arguments (%i) \
for symbol %s with arity %i" 
	(List.length tl) 
	(Symbol.get_name s)
	(Symbol.get_arity s);
      failwith "Atom.make"
    end
    else of_term (T (s, tl))
  with 
      Not_found -> failwith "Atom.make: unkown symbol";;


let rec parse_atom st = 
  (* location starts with this term *)
  assert (debug_b "Atom.parse_atom\n");
  restart_location ();
  let (s, k) = parse_symbol st in
    begin
      assert (debug_b "Atom.parse_atom: symbol = %s\n" s);
      if k <> PREDICATE
      then raise (parse_error "predicate symbol expected")
      else 
	(* nullary predicate *)
	if (is_empty st)
	then make (Symbol.make s 0 k) []
	  (* with args *)
	else if (Util.surepeek st) = '('
	then 
	  begin
	    parse_LP st;
	    let args = parse_args st in
	      parse_RP st;
	      make (Symbol.make s (List.length args) k) args 
	  end
        (* nullary predicate in clause *)
	else make (Symbol.make s 0 k) []		     
    end
and parse_LP st = 
  scan '(' st
and parse_RP st = 
  scan ')' st;;


let of_string s = 
  let st = Stream.of_string s in
    parse_atom st;;


(*****************************)
(* Access                    *)
(*****************************)

let get_predicate a = 
  try
    (Term.topsymbol (to_term a))
  with
      Failure _ -> failwith "atom.get_predicate";;


let check a = 
  Term.is_atom (to_term a);;


let get_args a = 
  Term.subterms (to_term a);;
  

let vars a = 
  Term.vars (to_term a);;


let occur x a = 
  Term.occur x (to_term a);;



let size a = 
  a.at_size;;


let equal a1 a2 = 
  Term.equal (to_term a1) (to_term a2);;





(*****************************)
(* Transformation            *)
(*****************************)


let select a = 
  { 
    at_atom = a.at_atom;
    at_size = a.at_size;
    at_selected = true;
  };;


let unselect a = 
  { 
    at_atom = a.at_atom;
    at_size = a.at_size;
    at_selected = false;
  };;


let var_transform op a = 
  { 
    at_atom = op a.at_atom;
    at_size = a.at_size;
    at_selected = a.at_selected;
  };;


let rename r = 
  var_transform (Term.rename r);;


let refresh = 
  var_transform Term.refresh;;


let map_var f = 
  var_transform (Term.map_var f);;


let substitute s a = 
  let t1 = Substitution.substitute s (to_term a)
  in { 
      at_atom = t1;
      at_size = Term.size t1;
      at_selected = a.at_selected;
    };;


let unify a b = 
  Unification.unify (to_term a) (to_term b);;


let matcher a1 a2 = 
  Matching.matcher (to_term a1) (to_term a2);;


let matching a1 a2 = 
  Matching.matching (to_term a1) (to_term a2);;


let normalize r a = 
  of_term (Rewriting.normalize r (to_term a));;


let exists_var p a = 
  Term.exists_var p (to_term a);;


let for_all_var p a = 
  Term.for_all_var p (to_term a);;


let is_deep a = 
  exists (fun t -> not (Term.is_variable t)) (get_args a);;


let is_flat a = 
  (get_kind (get_predicate a) = PREDICATE)
  && (for_all Term.is_variable (get_args a));;


let is_epsilon a = 
(*  assert (debug_b "Atom.is_epsilon %s = %s\n" 
	    (to_string a)
	    (string_of_bool ((is_flat a) && (get_arity (get_predicate a) = 1)))
	 ); *)
  (is_flat a) 
  && (get_arity (get_predicate a) = 1);;
(* (assert (List.length (get_args a) = 1);  *)

    
let is_splitting a = 
  Symbol.get_kind (get_predicate a) = SPLITTING;;



