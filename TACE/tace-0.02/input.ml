
open List;;
open Stream;;

open Trace;;
open Util;;
open Location;;
open Parsing;;
open Symbol;;
open Atom;;
open Eblocks;;
open Clause;;
open Clausetype;;
open Trs;;




(**********************************)
(* detect and mark test predicates*)
(**********************************)


(** c must be a Horn clause with a non-equational and
    non-splitting positive literal. *)
let head_predicate_opt c = 
  let al = Clause.get_others POS c in
    if List.length al = 1
    then 
      Some (Atom.get_predicate (List.hd al))
    else 
      None;;


let add_predicate p pl = 
  assert (get_kind p = PREDICATE);
  clean_ins Symbol.equal p pl;;


(** [get_testpredicate cl]
    @return the test predicates of the clause list [cl]
    (head predicateof clauses corresponding to a constrained transition). *)
let get_testpredicates cl = 
  let rec get_tp1 pl = function
      [] -> pl
    | c::l -> match (get_type c) with
	  UNDEF -> 
	    get_tp1 pl l
	| EQ 
	(* we allow clauses with deep negative literal in the initial file *)
	| DEEP -> 
	    (match (head_predicate_opt c) with
	         None -> 
		   get_tp1 pl l
	       | Some p ->
		   assert (debug_b "test predicate: %s\n" 
			     (Symbol.to_string p));
		   get_tp1 (add_predicate p pl) l)
	(* this kind of clause should not be in the initial file *)
	| SPOS -> 
	    warning "Parsefile.get_testpredicates: split positive clause\n";
	    get_tp1 pl l
	| REG -> 
	    get_tp1 pl l
	(* this kind of clause should not be in the initial file *)
	| SPLIT -> 
	    warning "Parsefile.get_testpredicates: split clause\n";
	    get_tp1 pl l
	(* we allow epsilon-transition in the initial file *)
	| ONEVAR ->
	    get_tp1 pl l	
	(* for the query *)
	| UGOAL -> 
	    get_tp1 pl l		  
	(* this kind of clause should not be in the initial file *)
	| UNSEL -> 
	    warning "Parsefile.get_testpredicates: clause of type UNSEL\n";
	    (match (head_predicate_opt c) with
	         None -> 
		   get_tp1 pl l
	       | Some p ->
		   assert (debug_b "test predicate: %s\n" 
			     (Symbol.to_string p));
		   get_tp1 (add_predicate p pl) l)
  in get_tp1 [] cl;;


(** @return the predicate [p] with 
    flag test_predicate set if [p]
    belongs to [pl]. Otherwise return [p]. *)
let symbol_set_testpredicate pl p = 
  if (get_kind p = PREDICATE) && (Util.meme Symbol.equal p pl)
  then 
    if (Symbol.is_testpredicate p)
    then begin
      warning "set_testpredicates: symbol %s already test predicate\n"
	(Symbol.to_string p);
      p
    end
    else Symbol.set_testpredicate p
  else p;;


let atom_set_testpredicate pl a  =
  assert (Atom.check a);
  let p = (Atom.get_predicate a) 
  in Atom.make (symbol_set_testpredicate pl p) (Atom.get_args a);;

    
let clause_set_testpredicate pl c =
  Clause.make
    (get_splittings NEG c)
    (List.map (atom_set_testpredicate pl) (get_others NEG c))
    (get_equations NEG c)
    (get_splittings POS c)
    (List.map (atom_set_testpredicate pl) (get_others POS c))
    (get_equations POS c);;


(**********************************)
(* preprocesses list of clauses   *)
(**********************************)


let separate cl = 
  let rec separate1 r rest = function
      [] -> (r, rest)
    | c::l -> 
	if Clause.is_equation c
	then 
	  separate1 (Trs.add (Clause.to_equation c) r) rest l
	else 
	  separate1 r (c::rest) l
  in separate1 (Trs.make ()) [] cl;;


let import_clauses cl = 
  let pl = get_testpredicates cl in
  let cl1 = List.map (clause_set_testpredicate pl) cl
  in separate cl1;;
   
  
(**********************************)
(* parse file                     *)
(**********************************)


let parse_until st c = 
  while not ((Util.is_empty st) || (Util.surepeek st) == c)
  do
    skip st
  done;;


let parse_comment st = 
  scan '#' st;
  parse_until st '\n';;


(** We detect and mark the test predicates properly
    (see {! Symbol.is_testpredicate} and {! Resolution}). *)
let import_file fn = 
  try 
    let ch = Parsing.set_filename fn; open_in fn in
    let st = Stream.of_channel ch in
    let cl = ref ([]) in
      debug "start parsing %s\n" fn;
      while not (Util.is_empty st)
      do
	match (Util.surepeek st) with 
	    ' '
	  | '\t'
	  | '\n' -> 
	      skip st
	  | '#' ->
	      parse_comment st
	  | _ -> 
	      let c = parse_clause st in
		assert (debug_b "Parsefile.to_clauses: clause parsed: %s\n"
			  (Clause.to_stringt c));
		cl := c::!cl
      done;
      import_clauses !cl
  with
      Sys_error msg -> 
	error "System Error: %s\n" msg;
	error "Import.import_file: could not open file %s\n" fn;
	failwith "Import.import_file"
    | Parse_error (msg, loc) ->
	error "%s: %s\n" (Location.to_string loc) msg;
	failwith "Import.import_file";;


      
      
