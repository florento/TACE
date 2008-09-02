
open List;;
open Hashtbl;;
open Printf;;

open Trace;;
open Util;;
open Symbol;;
open Variable;;
open Eblocks;;
open Clausetype;;
open Clause;;


(** association list of the nullary splitting predicate created *)
let esplit_tbl = 
  ref [];;


let cpt = ref 0;;


(** TBR useless, we do not consider the flags for symbols equality. *)
(** the same predicate can have 4 flags set or not.
    with this function, we unset all flags. *)
let predicate_unflag q = 
  unset_flag 4 (unset_flag 3 (unset_flag 2 (unset_flag 1 q)));;


let new_splitting () = 
  let q = Symbol.make ("q"^(string_of_int !cpt)) 0 SPLITTING in
    debug "Esplitting.new_splitting predicate %s\n" (Symbol.to_string q);
    cpt := !cpt+1;
    q;;


let rec check_tbl = function
    [] -> true
  | (ql, s)::l -> 
      (List.for_all (fun (_, s2) -> not (Symbol.equal s2 s)) l)
      && (is_clean Symbol.equal ql)
      && (List.for_all (fun (ql2, _) -> not (list_equal Symbol.equal ql2 ql)) l)
      && check_tbl l;;


(* optimisation
   if a esplitting nullary q has already been created for Q1,...,Qn,
   then the clause Q1(x),..., Qn(x) => q has already been add.
   We don't rebuilt it. *)
type make_answer = 
    ESP_NEW of (symbol list) * symbol
  | ESP_ALREADY of symbol;;


let make_split_atom p = 
  assert (get_kind p = SPLITTING);
  assert (get_arity p = 0);
  Atom.make p [];;


let make_clause1 ql q = 
  assert (get_kind q = SPLITTING);
  assert (get_arity q = 0);  
  let x = Variable.make 1 in
  let make_epsilon p = 
    assert (get_kind p = PREDICATE);
    assert (get_arity p = 1);
    Atom.make p [ Term.of_variable x ] in	 
  let al = List.map make_epsilon ql in
  let b = make_split_atom q 
  in Clause.make [] al [] [b] [] [];;
  

(* make_predicate *)
let make_predicate ql = 
  debug "Esplitting.make %s \n" 
    (Util.list_to_string Symbol.to_string ql);
  assert (is_clean Symbol.equal ql);
  assert (check_tbl !esplit_tbl);
  (* let sql = List.map predicate_unflag ql in *)
  if mem_assoce (fun l1 l2 -> list_equal Symbol.equal l1 l2) ql !esplit_tbl
  then 
    ESP_ALREADY (assoce (list_equal Symbol.equal) ql !esplit_tbl)
  else 
    let q = (new_splitting ()) in 
      esplit_tbl := (ql, q)::!esplit_tbl; 
      ESP_NEW (ql, q);;


let get_predicates q = 
  try
    fst (List.find (fun (_, q1) -> Symbol.equal q1 q) !esplit_tbl)
  with
      Not_found -> 
	error "Esplitting.get_predicates: %s not found\n" (Symbol.to_string q);
	[];;


let make_clause q = 
  assert (debug_b "Esplitting.make_clause: %s\n" (Symbol.to_string q));
  assert (get_kind q = SPLITTING);
  assert (get_arity q = 0);
  let ql = get_predicates q
  in make_clause1 ql q;;
	

let splittable c = 
  match (get_type c) with      
      UNDEF -> 
	true
    (* eclauses are not split, solve equations first! *)
    | EQ ->  
	assert (debug_b "Esplitting.esplit: %s equational, not split\n" 
		  (Clause.to_stringt c));
	false
    (* not splittable by definition *)
    | SPOS ->
	assert (debug_b "Esplitting.esplit: %s spos, not split\n" 
		  (Clause.to_stringt c));
	false
    (* regular clauses are not splittable by definition *)
    | REG -> 
	assert (debug_b "Esplitting.esplit: %s regular, not split\n" 
		  (Clause.to_stringt c));
	false
    | SPLIT
    | DEEP ->
	true
    (* onevar clauses are not splittable by definition *)
    | ONEVAR 
    | UGOAL ->
	assert (debug_b "Esplitting.esplit: %s one var., not split\n" 
		  (Clause.to_stringt c));
	false
    | UNSEL ->
	true;;
  

let separate mal = 
  let rec separate1 (cl, ql) = function
      [] -> (cl, ql)
    | (ESP_ALREADY q)::l -> 
	separate1 (cl, q::ql) l
    | (ESP_NEW (ql1, q1))::l -> 
	let c = make_clause1 ql1 q1 
	in separate1 (c::cl, q1::ql) l
  in separate1 ([], []) mal;;


let esplit c = 
  assert (debug_b "Esplitting.esplit %s\n" (Clause.to_stringt c));
  if not (splittable c)
  then begin
    assert (debug_b "Esplitting.esplit  %s: type not splittable\n" 
	      (Clause.to_stringt c));
    [ ]
  end
  else 
    let (ebl, c1) = extract_eblocks c in    
      assert (debug_b "Esplitting.esplit: eblocks = %s, clause = %s\n"
		(Eblocks.to_string ebl)
		(to_string c1));      
      if (Eblocks.is_empty ebl)
      then [ ]
      else 
	let pll = Eblocks.get_predicates ebl in
	let (cl, ql) = separate (List.map make_predicate pll) in
	let bl = List.map make_split_atom ql in
	let c2 = Clause.merge (Clause.make bl [] [] [] [] []) c1 in
	let cl1 = (c2::cl) in
	  (* TODO mv trace to module Saturate *)
	  (if (List.length cl1 > 1) && (get_trace_level () >= 4)
	   then trace 4 "clause %s split into: %s\n" 
	     (Clause.to_stringt c)
	     (Util.list_to_string Clause.to_stringt cl1));
	  cl1;;


let nb_esplitting_symbols () =
  !cpt;;


let dump () = 
  printf "%i esplitting predicates\n" (List.length !esplit_tbl);
  printf "cpt = %i\n" !cpt;  
  printf "%-6s  %s\n" "symbol" "predicates";
  List.iter
    (fun (ql, p) -> 
       printf "%-6s  %s\n" 
	 (Symbol.to_string p)
	 (Util.list_to_string Symbol.to_string ql))
    !esplit_tbl;;


