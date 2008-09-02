
open List;;
open Printf;;
open Format;;
open Stream;;

open Trace;;
open Util;;
open Parsing;;
open Symbol;;
open Atom;;
open Equation;;
open Clausetype;;
open Substitution;;


type sign = 
    POS
  | NEG;;


let sign_to_string = function
    POS -> "POS"
  | NEG -> "NEG";;
    

type literals = {
  li_split : atom list;
  (** atoms built with splitting predicate *)
  li_atom  : atom list;
  (** atoms built with other predicate *)
  li_eq    : equation list;
  (** equational literals *)
};;


type clause = { 
  cl_neg : literals;
  (** negative literals *)
  cl_pos : literals;
  (** positive literals *)
  cl_size  : int;
  (** size of the clause, should always be positive. *)
  cl_type  : clausetype;
  (** type of the clause. *)
};;
    


(***************************)
(* Output                  *)
(***************************)

let rec to_string c =
  let nsl = 
    (List.map Atom.to_string c.cl_neg.li_split)
    @(List.map Atom.to_string c.cl_neg.li_atom)
    @(List.map Equation.to_string c.cl_neg.li_eq) in
  let psl = 
    (List.map Atom.to_string c.cl_pos.li_split)
    @(List.map Atom.to_string c.cl_pos.li_atom)
    @(List.map Equation.to_string c.cl_pos.li_eq) 
  in sprintf "%s => %s" 
       (list_to_string (fun s -> s) nsl)
       (list_to_string (fun s -> s) psl);;


let to_stringt c =
  sprintf "[%s] %s" (Clausetype.to_string c.cl_type) (to_string c);;


let dump6 nsl nal nel psl pal pel = 
  open_box 0; 
  print_string "neg split = ";
  Util.dump_list (fun a -> print_string (Atom.to_string a)) nsl;
  close_box ();
  print_newline ();
  open_box 0; 
  print_string "neg atoms = ";
  Util.dump_list (fun a -> print_string (Atom.to_string a)) nal;
  close_box ();
  print_newline ();
  open_box 0; 
  print_string "neg eq = ";
  Util.dump_list (fun e -> print_string (Equation.to_string e)) nel;
  close_box ();
  print_newline ();
  open_box 0; 
  print_string "pos split = ";
  Util.dump_list (fun a -> print_string (Atom.to_string a)) psl;
  close_box ();
  print_newline ();
  open_box 0; 
  print_string "pos atoms = ";
  Util.dump_list (fun a -> print_string (Atom.to_string a)) pal;
  close_box ();
  print_newline ();
  open_box 0; 
  print_string "pos eq = ";
  Util.dump_list (fun e -> print_string (Equation.to_string e)) pel;
  close_box ();;


let dump c = 
  open_box 0; 
  print_string "clause (";
  print_string "type = ";
  print_string (Clausetype.to_string c.cl_type);
  print_char ',';
  print_space ();
  print_string "size = ";
  print_int c.cl_size;
  print_string "):";
  print_newline ();
  dump6 
    c.cl_neg.li_split c.cl_neg.li_atom c.cl_neg.li_eq
    c.cl_pos.li_split c.cl_pos.li_atom c.cl_pos.li_eq;
  print_newline ();
  close_box ();;


(***************************)
(* Literals                *)
(***************************)


(** equal2 implies equal1 *)
let rec insert_ord bigger equal1 equal2 a = function
    [] -> [a]
  | b::l as al -> 
      if bigger a b
      then a::al
      else if equal1 a b
      then 
	if equal2 a b
	then al
	else a::al
      else b::(insert_ord bigger equal1 equal2 a l);;


let insert_atom = 
  insert_ord 
    (fun a1 a2 -> (Atom.size a1) > (Atom.size a2))
    (fun a1 a2 -> (Atom.size a1) = (Atom.size a2))
    Atom.equal;;


let insert_equation = 
  insert_ord 
    (fun a1 a2 -> (Equation.size a1) > (Equation.size a2))
    (fun a1 a2 -> (Equation.size a1) = (Equation.size a2))
    Equation.equal;;


let literals_empty () = 
  {
    li_split = [];
    li_atom = [];
    li_eq = [];
  };;


(* suppress double *)
let literals_make sl al el = 
  {
    li_split = fold_right insert_atom sl [];
    li_atom = fold_right insert_atom al [];
    li_eq = fold_right insert_equation el [];
  };;


let literals_is_empty l = 
  (l.li_split = []) 
  && (l.li_atom = []) 
  && (l.li_eq = []);;


let literals_has_splitting l = 
  (l.li_split <> []) 
  && (l.li_atom = []) 
  && (l.li_eq = []);;


(** [literals_length l]
    @return the number of literals in [l]. *)
let literals_length l = 
  (List.length l.li_split)
  + (List.length l.li_atom)
  + (List.length l.li_eq);;


let literals_size l = 
  (size_list Atom.size l.li_split) 
  + (size_list Atom.size l.li_atom) 
  + (size_list Equation.size l.li_eq);;


let literals_insert_split a l = 
  {
    li_split = insert_atom a l.li_split;
    li_atom  = l.li_atom;
    li_eq    = l.li_eq;
  };;


let literals_insert_atom a l = 
  {
    li_split = l.li_split;
    li_atom  = insert_atom a l.li_atom;
    li_eq    = l.li_eq;
  };;


let literals_insert_eq e l = 
  {
    li_split = l.li_split;
    li_atom  = l.li_atom;
    li_eq    = insert_equation e l.li_eq;
  };;


(** we keep the literals lists:
    - without repetition and
    - ordered by decreasing sizes *)
let literals_union l1 l2 = 
  {
    li_split = fold_right insert_atom l1.li_split l2.li_split;
    li_atom = fold_right insert_atom l1.li_atom l2.li_atom;
    li_eq = fold_right insert_equation l1.li_eq l2.li_eq;
  };;


let literals_occur x l = 
  (List.exists (Atom.occur x) l.li_split)
  || (List.exists (Atom.occur x) l.li_atom)
  || (List.exists (Equation.occur x) l.li_eq);;

  
(* select the first deep literal met *)
let rec select_deep = function
    [] -> []
  | a::l ->
      if (is_deep a)
      then (Atom.select a)::(List.map Atom.unselect l)
      else let a1 = (Atom.unselect a) in a1::(select_deep l);;


(** selection in negative literals *)
let literals_select nl = 
  if nl.li_eq <> []
  then 
    {
      li_split = List.map Atom.unselect nl.li_split; 
      li_atom  = List.map Atom.unselect nl.li_atom;
      li_eq    = nl.li_eq;
    }
  else if nl.li_split <> []
  then 
    {
      li_split = List.map Atom.select nl.li_split; 
      li_atom  = List.map Atom.unselect nl.li_atom;
      li_eq    = nl.li_eq;
    }
  else 
    {
      li_split = nl.li_split; 
      li_atom  = select_deep nl.li_atom;
      li_eq    = nl.li_eq;
    };;
    

(***************************)
(* Access                  *)
(***************************)

let size c = 
  c.cl_size;;


let get_type c = 
  c.cl_type;;


let length c = 
  (literals_length c.cl_pos) + (literals_length c.cl_neg);;


let lengths s c = 
  match s with
      POS -> literals_length c.cl_pos
    | NEG -> literals_length c.cl_neg;;


let get_equations s c = 
  match s with 
      NEG -> c.cl_neg.li_eq
    | POS -> c.cl_pos.li_eq;;


let get_splittings s c = 
  match s with 
      NEG -> c.cl_neg.li_split
    | POS -> c.cl_pos.li_split;;


let get_others s c = 
  match s with 
      NEG -> c.cl_neg.li_atom
    | POS -> c.cl_pos.li_atom;;


let get_atoms s c = 
  rev_append (get_splittings s c) (get_others s c);;


(*************************************)
(* Access special for TA transitions *)
(*************************************)


let reg_get_target c = 
  assert (c.cl_type = REG);
  assert (List.length c.cl_pos.li_atom = 1);
  let q = Atom.get_predicate (hd c.cl_pos.li_atom) in 
    assert (get_kind q = PREDICATE);
    assert (get_arity q = 1);
    q;;


let reg_get_function c = 
  assert (c.cl_type = REG);
  assert (List.length c.cl_pos.li_atom = 1);
  assert (List.length (get_args (hd c.cl_pos.li_atom)) = 1);
  let f = Term.topsymbol (hd (Atom.get_args (hd c.cl_pos.li_atom))) in
    assert (get_kind f = FUNCTION);
    f;;


let  reg_get_args c = 
  assert (c.cl_type = REG);
  assert (List.length c.cl_pos.li_atom = 1);
  assert (List.length c.cl_neg.li_atom = get_arity (reg_get_function c));
  List.map 
    (fun a -> 
       let q = Atom.get_predicate a in 
	 assert (get_kind q = PREDICATE);
	 assert (get_arity q = 1);
	 q)
    c.cl_neg.li_atom;;



(************************************)
(* Separate tests & type evaluation *)
(************************************)


let is_horn6 nsl nal nel psl pal pel =
  (List.length psl) + (List.length pal) + (List.length pel) <= 1;;


let is_eq6 nsl nal nel psl pal pel =
  nel <> [];;


let is_positive6 nsl nal nel psl pal pel =
  (nsl = []) && (nal = []) && (nel = []);;


let is_spos6 nsl nal nel psl pal pel =
  (is_positive6 nsl nal nel psl pal pel)
  && (List.length psl = 1)
  && (pal = [])
  && (pel = []);;


let is_reg6 nsl nal nel psl pal pel =
  (psl = [])
  && (List.length pal = 1)
  && (pel = [])
  && (nsl = [])
  && (for_all Atom.is_epsilon nal)
  && (nel = [])
  && (let pa = hd pal in
      let tl = Atom.get_args pa 
      in (List.length tl = 1)
  && (let t = hd tl in
      let ul = Term.subterms t 
      in (for_all Term.is_variable ul)
  && (let xl = (List.map Term.to_variable ul) in
      let yl = (List.map (fun a -> Term.to_variable (hd (get_args a)))) nal 
      in (List.length xl = List.length yl)
     (* term is head is linear *)
  && (is_clean Variable.equal xl)
     (* body is linear *)
  && (is_clean Variable.equal yl)
  && (list_equal Variable.equal xl yl))));;


let is_split6 nsl nal nel psl pal pel =
  (nsl <> []);;


let is_deep6 nsl nal nel psl pal pel =
  (exists (Atom.is_deep) nal);;


let all_equal eq = function
    [] -> true
  | a::l -> List.for_all (fun x -> eq x a) l;;


let is_onevar6 nsl nal nel psl pal pel =
  (nsl = [])
  && (nel = [])
  && (for_all Atom.is_epsilon nal)
  && (psl = [])
  && (pel = [])
  && (List.length pal = 1)
  && (Atom.is_epsilon (hd pal))
  && (let extract_var = (fun a -> Term.to_variable (hd (get_args a))) in
      let xl = (List.map extract_var nal) in
      let yl = (List.map extract_var pal) in 
     (* List.length yl = 1 *)
     (all_equal Variable.equal ((hd yl)::xl)));;


let is_ugoal6 nsl nal nel psl pal pel =
  (nsl = [])
  && (nel = [])
  && (pal = [])
  && (pel = [])
  && (List.length psl <= 1)
  && (for_all Atom.is_epsilon nal)
  && (let xl = (List.map (fun a -> Term.to_variable (hd (get_args a))) nal) in
      if (List.length xl > 0)
      then 
	let x = (hd xl) 
	in (for_all (Variable.equal x) xl)
           (* empty clause, not type SPOS *)
      else (List.length psl = 0));; 


let is_unsel6 nsl nal nel psl pal pel =
  (nsl = [])
  && (nel = [])
  && (for_all Atom.is_epsilon nal)
  && (psl = [])
  && (pel = [])
  && (pal <> [])
  && (for_all Atom.is_deep pal);;


let eval_type6 nsl nal nel psl pal pel =
  assert (List.for_all Atom.is_splitting nsl);
  assert (List.for_all (fun a -> not (Atom.is_splitting a)) nal);
  assert (List.for_all Atom.is_splitting psl);
  assert (List.for_all (fun a -> not (Atom.is_splitting a)) pal);
  if is_eq6 nsl nal nel psl pal pel
  then EQ
  else if is_spos6 nsl nal nel psl pal pel
  then SPOS
  else if is_reg6 nsl nal nel psl pal pel
  then REG
  else if is_split6 nsl nal nel psl pal pel
  then SPLIT
  else if is_deep6 nsl nal nel psl pal pel
  then DEEP
  else if is_onevar6 nsl nal nel psl pal pel
  then ONEVAR
  else if is_ugoal6 nsl nal nel psl pal pel
  then UGOAL
  else if is_unsel6 nsl nal nel psl pal pel
  then UNSEL
  else begin
(*
    warning "Clause.eval_type6: unknown type for %s\n"
      (to_string {
	 cl_neg = literals_make nsl nal nel;
	 cl_pos = literals_make psl pal pel;
	 cl_size = 0;
	 cl_type = UNDEF });	   
*)
    UNDEF
  end;;


let eval_type2 nl pl = 
  eval_type6 
    nl.li_split nl.li_atom nl.li_eq
    pl.li_split pl.li_atom pl.li_eq;;


let eval_type c = 
  eval_type2 c.cl_neg c.cl_pos;;


let check_type c = 
  Trace.check ((eval_type c) = (c.cl_type))
    "Clause.check: type %s <> stored type %s\n"
    (Clausetype.to_string (eval_type c)) 
    (Clausetype.to_string c.cl_type);;


(***************************)
(* Size evaluation         *)
(***************************)


let eval_size6 nsl nal nel psl pal pel =
  (size_list Atom.size nsl) +
  (size_list Atom.size nal) +
  (size_list Equation.size nel) +
  (size_list Atom.size psl) +
  (size_list Atom.size pal) +
  (size_list Equation.size pel);;


let eval_size2 nl pl =
  (literals_size nl) + (literals_size pl);;


let eval_size c =
  eval_size2 c.cl_neg c.cl_pos;;


let check_size c =
  Trace.check ((eval_size c) = (c.cl_size))
    "Clause.check: size %i <> stored size %i\n" (eval_size c) (c.cl_size);;



(***************************)
(* Testers                 *)
(***************************)


let is_horn c = 
  literals_length c.cl_pos <= 1;;


let is_goal c = 
  literals_is_empty c.cl_pos;;


let is_positive c = 
  literals_is_empty c.cl_neg && (not (literals_is_empty c.cl_pos));;


let is_goal_or_splitting c = 
  (literals_is_empty c.cl_pos) || (literals_has_splitting c.cl_pos);; 


let is_empty c = 
  (literals_is_empty c.cl_pos) && (literals_is_empty c.cl_neg);;


let is_regular c = 
  c.cl_type = REG;;


let is_equation c =
  (is_positive c)
  && (c.cl_pos.li_split = [])
  && (c.cl_pos.li_atom = [])
  && (List.length c.cl_pos.li_eq = 1);;



(***************************)
(* Construction            *)
(***************************)


(* if applied without splitting, the resulting type can be unknown *)
let merge c1 c2 = 
  let nl = literals_union c1.cl_neg c2.cl_neg in 
  let pl = c2.cl_pos in
    {
      cl_neg = literals_select nl;
      cl_pos = pl;
      cl_size = eval_size2 nl pl;
      cl_type = eval_type2 nl pl;
    };;


(** recompute the clause type and selection of negative literals *)
let cast c = 
  assert (debug_b "Clause.cast %s\n" (to_string c));
  { 
    cl_neg = literals_select c.cl_neg;
    cl_pos = c.cl_pos;
    cl_size = c.cl_size;
    cl_type = eval_type c;
  };;
  

let empty () =
  assert (debug_b "Clause.empty\n");
  { 
    cl_neg = literals_empty ();
    cl_pos = literals_empty ();
    cl_size = 0;
    cl_type = UGOAL;
  };;


let rawmake nl pl = 
  let c = 
    {
      cl_neg = literals_select nl;
      cl_pos = pl;
      cl_size = eval_size2 nl pl;
      cl_type = eval_type2 nl pl;
    } in
    assert (check_size c);
    assert (check_type c);
    c;;


let make nsl nal nel psl pal pel = 
  assert (debug_b "Clause.make\n");
  assert (List.for_all Atom.is_splitting nsl);
  assert (List.for_all (fun a -> not (Atom.is_splitting a)) nal);
  assert (List.for_all Atom.is_splitting psl);
  assert (List.for_all (fun a -> not (Atom.is_splitting a)) pal);
  let nl = literals_make nsl nal nel in
  let pl = literals_make psl pal pel
  in rawmake nl pl;;


let to_equation c = 
  assert (is_equation c);
  hd c.cl_pos.li_eq;;


let parse_clause st =
  assert (debug_b "Clause.parse_clause\n");
  let rec parse_neg nl st = 
    match (Stream.peek st) with
	None -> 
	  error "Clause.parse_neg: atom or equation or '=>' expected";
	  raise (parse_error "atom or equation or '=>' expected")
      | Some c -> 
	  (match c with 
	       ' '
	     | '\t' 
	     | '\n' -> 
		 skip st; parse_neg nl st
	     | '=' -> 
		 parse_arrow nl st;
	     | 'a'..'z' 
	     | '0'..'9' -> 
		 let e = parse_equation st in
		   (* nel := e::!nel; *)
		 let nl1 = 
		   assert (debug_b "Clause.parse_neg: equation %s\n" 
			     (Equation.to_string e));
		   literals_insert_eq e nl 
		 in parse_nsep nl1 st
	     | 'A'..'Z' -> 
		 let a = parse_atom st in
		   (* nal := a::!nal; (parse_nsep st) *)
		 let nl1 = 
		   assert (debug_b "Clause.parse_neg: atom %s\n" 
			     (Atom.to_string a));
		   literals_insert_atom a nl 
		 in parse_nsep nl1 st
	     | _ -> 
		 error "Clause.parse_neg: unexpected character %c\n" c;
		 raise (parse_error "atom or equation or '=>' expected"))
  and parse_arrow nl st = 
    scan '=' st;
    scan '>' st;
    assert (debug_b "Clause.parse_arrow\n");
    let pl0 = literals_empty ()
    in parse_pos nl pl0 st
  (** parse separator for list of literals and continue parsing with [cont]
      or parse an arrow
      @param sub list of subterms already parsed *)
  and parse_nsep nl st = 
    assert (debug_b "Clause.parse_nsep\n");
    match (Stream.peek st) with
	None -> 
	  raise (parse_error "',' or '=>' expected")
      |  Some c -> 
	   (match c with 
		',' -> 
		  skip st; parse_neg nl st
	      | '=' -> 
		  parse_arrow nl st
	      | ' '
	      | '\t'
	      | '\n' -> 
		  skip st; parse_nsep nl st
	      | _ -> 
		  error "Clause.parse_nsep: unexpected character %c\n" c;
		  raise (parse_error "',' expected"))
  and parse_pos nl pl st =
    assert (debug_b "Clause.parse_pos\n");
    match (Stream.peek st) with
	None -> rawmake nl pl
      | Some c -> 
	  (match c with 
	       ' '
	     | '\t'
	     | '\n' -> 
		 skip st; parse_pos nl pl st
	     | '.' -> 
		 skip st; 
		 rawmake nl pl;
	     | 'a'..'z'
	     | '0'..'9' -> 
		 let e = parse_equation st in
		 let pl1 = 
		   assert (debug_b "Clause.parse_pos: equation %s\n"
			     (Equation.to_string e));
		   literals_insert_eq e pl 
		 in parse_psep 0 nl pl1 st
	     | 'A'..'Z' -> 
		 let a = parse_atom st in
		 let pl1 =
		   assert (debug_b "Clause.parse_pos: atom %s\n" 
			     (Atom.to_string a));
		   literals_insert_atom a pl 
		 in parse_psep 0 nl pl1 st
	     | _ -> 
		 error "Clause.parse_pos: unexpected character %c\n" c;
		 raise (parse_error "atom or equation expected")) 
  and parse_psep flag nl pl st = 
    match (Stream.peek st) with
	None -> 
	  rawmake nl pl (* EOF: end of clause parsing *)
      |  Some c -> 
	   (match c with 
		',' -> 
		  skip st; parse_pos nl pl st
	      | ' '
	      | '\t' 
	      | '\n' -> 
		  skip st; parse_psep flag nl pl st
	      | '.' -> 
		  skip st; 
		  rawmake nl pl;
	      | _ -> 
		  error "Clause.parse_psep: unexpected character %c\n" c;
		  raise (parse_error "',' expected")) in 
  let nl0 = literals_empty () in
  let c = parse_neg nl0 st in
    assert (debug_b "Clause.parse: parsed %s\n" (to_string c));
    c;;


let of_string s = 
  let st = Stream.of_string s
  in parse_clause st;;




let check c = 
  (for_all (fun a -> Atom.is_splitting a) c.cl_neg.li_split)
  && (for_all (fun a -> Atom.is_splitting a) c.cl_pos.li_split)
  && (for_all Atom.check c.cl_neg.li_atom) 
  && (for_all Atom.check c.cl_pos.li_atom)
  && (for_all Equation.check c.cl_neg.li_eq) 
  && (for_all Equation.check c.cl_pos.li_eq)
  && check_size c
  && check_type c;;



(***************************)
(* Selection               *)
(***************************)

let extract_all al = 
  let rec select_all1 first = function
      [] -> []
    | a::l -> 
	(* pair [a], all equations but the selected [a] *)
	let p1 = (a, rev_append first l)
	in p1::(select_all1 (a::first) l)
  in select_all1 [] al;;
      

let extract_one = function
    [] -> []
  | a::l -> [ (a, l) ];;


(** [select_extract pred al]
    @param pred is a tester which returns [None]
    or [Some x]
    @return the list of pairs [(x, l)] such that
    [pred a] returns [Some x] on some the the atom [a] of [al],
    and [l] is [al] minus [a]. *)
let extract_pred pred al =
  let rec extract_pred1 first = function
      [] -> []
    | a::l -> 
	match (pred a) with
	    Some x ->
	  (* pair [a], all equations but the selected [a] *)
	      let p1 = (x, rev_append first l)
	      in p1::(extract_pred1 (a::first) l)
	  | None -> 
	      extract_pred1 (a::first) l
  in extract_pred1 [] al;;
     

(** [extract_selected al]
    @return the list of pairs [(a, l)] where:
    - if there exists at least a selected atom in [al], then
    [a] is a a selected atom of [al] and [l] is [al] minus [a],
    - otherwise [a] is an atom of [al] and [l] is [al] minus [a] *)
let extract_selected al = 
  if (List.exists Atom.is_selected al)
  then extract_pred (fun a -> 
		       if is_selected a 
		       then Some a
		       else None) al
  else extract_pred (fun a -> Some a) al;;


(** [extract_selected_unif b al]
    @return the list of triples [(a, l, s)] where:
    - if there exists at least a selected atom in [al],
    then [a] is a selected atom of [al] unifiable with [b],
    otherwise [a] is any atom of [al] unifiable with [b],
    - [l] is [al] minus [a]
    - [s] the mgu of [a] and [b]. *)
let extract_selected_unif a al = 
  if (List.exists Atom.is_selected al)
  then extract_pred (fun b -> 
		       if is_selected b 
		       then Atom.unify b a
		       else None) al
  else extract_pred (fun b -> Atom.unify b a) al;;


(* select one equation *)
let select_equations c = 
  let nal = c.cl_neg.li_atom in
  let nsl = c.cl_neg.li_split 
  in List.map 
       (fun (e, el) -> (e, rawmake (literals_make nsl nal el) c.cl_pos))
       (extract_one c.cl_neg.li_eq);;


(* select one splitting literal *)
let select_splitting c = 
  let nal = c.cl_neg.li_atom in
  let nel = c.cl_neg.li_eq in
  let pos = c.cl_pos in
  List.map 
    (fun (a, al) ->
       (a, 
	{
	  cl_neg = 
	    {
	      li_split = al;
	      li_atom  = nal;
	      li_eq    = nel;
	    };
	  cl_pos = pos;
	  (* size and type undefined *)
	  cl_size = 0;
	  cl_type = UNDEF;
	}))
    (extract_one c.cl_neg.li_split);;


(* select all the splitting literals unifiable with b *)
let select_splitting_unif b c = 
  let nal = c.cl_neg.li_atom in
  let nel = c.cl_neg.li_eq in
  let pos = c.cl_pos in
  List.map 
    (fun (s, al) -> 
       (s, 
	{
	  cl_neg = 
	    {
	      li_split = al;
	      li_atom  = nal;
	      li_eq    = nel;
	    };
	  cl_pos = pos;
	  (* size and type undefined *)
	  cl_size = 0;
	  cl_type = UNDEF;
	}))
    (extract_selected_unif b c.cl_neg.li_split);;

(* select the selected literals
   or if there are none, all the literals *)
let select_others c = 
  let nsl = c.cl_neg.li_split in
  let nel = c.cl_neg.li_eq in
  let pos = c.cl_pos in
    List.map 
      (fun (a, al) ->
	 (a, 
	  {
	    cl_neg = 
	      {
		li_split = nsl;
		li_atom  = al;
		li_eq    = nel;
	      };
	    cl_pos = pos;
	    (* size and type undefined *)
	    cl_size = 0;
	    cl_type = UNDEF;
	  }))      
      (extract_selected c.cl_neg.li_atom);;


let select_others_unif b c = 
  let nsl = c.cl_neg.li_split in
  let nel = c.cl_neg.li_eq in
  let pos = c.cl_pos in
  (* select all the literals unifiable with b *)
  List.map 
    (fun (s, al) -> 
       (s, 
	{
	  cl_neg = 
	    {
	      li_split = nsl;
	      li_atom  = al;
	      li_eq    = nel;
	    };
	  cl_pos = pos;
	  (* size and type undefined *)
	  cl_size = 0;
	  cl_type = UNDEF;
	}))
    (extract_selected_unif b c.cl_neg.li_atom);;


let select_atoms c =
  assert (c.cl_neg.li_eq = []); (* error "select_atoms: some equations\n" *)
  if c.cl_neg.li_split <> []
  then select_splitting c
  else select_others c;;


let select_atoms_unif b c =
  assert (c.cl_neg.li_eq = []); (* error "select_atoms: some equations\n" *)
  if is_splitting b
  then if c.cl_neg.li_split = [] then [] else select_splitting_unif b c
  else if c.cl_neg.li_split <> [] then [] else select_others_unif b c;;



(**************************)
(* Epsilon-Splitting      *)
(**************************)


(** [free_var x c]
    @return true iff [x] occurs only in negative atoms of the form [Q(x)]. *)
let bound_var x c =
  (literals_occur x c.cl_pos)
  || (List.exists (Atom.occur x) c.cl_neg.li_split)
  || (List.exists (Equation.occur x) c.cl_neg.li_eq)
  || (List.exists 
	(fun a -> (Atom.occur x a) && (not (Atom.is_epsilon a)))
	c.cl_neg.li_atom);;


let free_epsilon a c = 
  (is_epsilon a)
  && 
    let xl = Atom.vars a in
    let x = assert (List.length xl = 1); hd xl
    in not (bound_var x c);; 


(*
let extract_eblocks c = 
  extract_pred (fun a -> 
		  if (free_epsilon a c)
		  then Some a
		  else None) c.cl_neg.li_atom;;
*)


let extract_eblocks c = 
  assert (debug_b "Clause.extract_eblocks: %s\n" (to_string c));
  let rec extract_eblocks1 eb1 rest = function
      [] -> (eb1, List.rev rest)
    | a::l -> 
	if free_epsilon a c
	then extract_eblocks1 (Eblocks.add_atom a eb1) rest l
	else extract_eblocks1 eb1 (a::rest) l in
  let (ebl, nal) = extract_eblocks1 (Eblocks.make ()) [] c.cl_neg.li_atom in
  let ln = 
    {
      li_split = c.cl_neg.li_split;
      li_atom = nal;
      li_eq = c.cl_neg.li_eq;
    } in
  let lp = c.cl_pos in
  let c1 = rawmake ln lp
  in (ebl, c1);;



(*******************************)
(* Transformations             *)
(*******************************)


let for_all atom_test eq_test c = 
  List.for_all atom_test c.cl_neg.li_split 
  && List.for_all atom_test c.cl_neg.li_atom
  && List.for_all eq_test c.cl_neg.li_eq  
  && List.for_all atom_test c.cl_pos.li_split
  && List.for_all atom_test c.cl_pos.li_atom
  && List.for_all eq_test c.cl_pos.li_eq;;


let filter atom_filter eq_filter c = 
  let nsl = List.filter atom_filter c.cl_neg.li_split in
  let nal = List.filter atom_filter c.cl_neg.li_atom in
  let nel = List.filter eq_filter c.cl_neg.li_eq in   
  let psl = List.filter atom_filter c.cl_pos.li_split in
  let pal = List.filter atom_filter c.cl_pos.li_atom in
  let pel = List.filter eq_filter c.cl_pos.li_eq in
  let nl = literals_make nsl nal nel in
  let pl = literals_make psl pal pel 
  in rawmake nl pl;;


(** [map atom_op eq_op flag_size flag_type]
    @param flag_size true iff size must be reevaluated
    @param flag_type true iff type must be reevaluated *)
let map atom_op eq_op flag_size flag_type c = 
  let nsl = List.map atom_op c.cl_neg.li_split in
  let nal = List.map atom_op c.cl_neg.li_atom in
  let nel = List.map eq_op c.cl_neg.li_eq in   
  let psl = List.map atom_op c.cl_pos.li_split in
  let pal = List.map atom_op c.cl_pos.li_atom in
  let pel = List.map eq_op c.cl_pos.li_eq in
  let c1 = 
    { 
      cl_neg = 
	{
	  li_split = nsl;
	  li_atom  = nal;
	  li_eq    = nel;
	};
      cl_pos = 
	{
	  li_split = psl;
	  li_atom  = pal;
	  li_eq    = pel;
	};
      cl_size = 
	if flag_size  
	then eval_size6 nsl nal nel psl pal pel
	else c.cl_size;
      cl_type = 
	if flag_type  
	then eval_type6 nsl nal nel psl pal pel
	else c.cl_type;
    } in
    assert (check_size c1);
    assert (check_type c1);
    c1;;
  

let map_var f c = 
  map (Atom.map_var f) (Equation.map_var f) false false c;;


let for_all_var f c = 
  for_all (Atom.for_all_var f) (Equation.for_all_var f) c;;


let refresh c = 
  let ren = Renaming.make () 
  in map (Atom.rename ren) (Equation.rename ren) false false c;;


let substitute s c = 
  assert (debug_b "Clause.substitute %s\n" (to_string c));
  (* TODO normalize ? *)
  map (Atom.substitute s) (Equation.substitute s) true true c;;

