
open List;;
open Printf;;
open Stream;;

open Trace;;
open Util;;
open Parsing;;
open Symbol;;
open Variable;;


type term =
    T of symbol * term list
  | V of variable;;


let make s tl = 
  try
    if (Symbol.get_arity s) = (List.length tl)
    then T (s, tl)
    else failwith 
      (sprintf "Term.make: unexpected number of arguments (%i) \
                for symbol %s with arity %i" 
	 (List.length tl) 
	 (Symbol.get_name s)
	 (Symbol.get_arity s))
  with 
      Not_found -> failwith "Term.make: unkown symbol";;


let of_variable x = 
  V x;;


let is_alphanum = function
    'a'..'z'
  | 'A'..'Z'
  | '0'..'9'
  | '_' -> true
  | _ -> false;;


let rec parse_term st = 
  (* location starts with this term *)
  restart_location ();
  (* determine whether the first id is a variable or a symbol *)
  let lc2 = Stream.npeek 2 st in match lc2 with
      ['x' | 'y' | 'z'] 
    | ['x' | 'y' | 'z'; '0'..'9' ] ->
	assert (debug_b "Term.parse_term: var = %c...\n" (hd lc2));
	of_variable (parse_variable st)
    | ['x' | 'y' | 'z'; c ] when not (is_alphanum c) ->
	assert (debug_b "Term.parse_term: var = %c...\n" (hd lc2));
	of_variable (parse_variable st)
      (* non variable symbol *)
    | _ ->  
	let (s, k) = parse_symbol st in
	  assert (debug_b "Term.parse_term: symbol = %s\n" s);
	  (* nullary symbol *)
	  if (is_empty st)
	  then make (Symbol.make s 0 k) []
	  (* with args *)
	  else if (Util.surepeek st) = '('
	  then 
	    begin
	      assert (debug_b "Term.parseTerm: ID = %s\n%!" s);
	      parse_LP st;
	      let args = parse_args st in
		parse_RP st;
		make (Symbol.make s (List.length args) k) args 
	    end
	    (* constant *)
	  else make (Symbol.make s 0 k) []		     
and parse_LP st = 
  scan '(' st
and parse_RP st = 
  scan ')' st
(** parse list of terms (arguments) 
    @param sub list of subterms already parsed *)
and parse_args st = 
  match (Stream.peek st) with
      None -> raise (parse_error "term expected")
    | Some c -> 
	 (match c with 
	      ')'
	    | '(' 
	    | ',' -> raise (parse_error "term expected")
	    | ' '
	    | '\t' 
	    | '\n' -> skip st; parse_args st
	    | _ -> let t = (parse_term st) in t::(parse_sep st))
(** parse separator in a list of terms (arguments) 
    @param sub list of subterms already parsed *)
and parse_sep st = 
  match (Stream.peek st) with
      None -> raise (parse_error "',' or ')' expected")
    | Some c -> 
	 (match c with 
	      ',' ->  skip st; parse_args st
	    | ' '
	    | '\t'
	    | '\n' -> skip st; parse_sep st
	    | ')' ->  []   (* no skip *)
	    | _ -> raise (parse_error "',' or ')' or term expected"));;


let of_string s = 
  let st = Stream.of_string s in
    parse_term st;;


let rec to_string = function
    V x -> Variable.to_string x
  | T (sy, tl) -> 
      let s = (Symbol.to_string sy) in 
	if (Symbol.get_arity sy) = 0
	then s
	else sprintf "%s(%s)" s (list_to_string to_string tl);;


let rec rename ren = function
    V x -> 
      V (Renaming.apply ren x)
  | T (f, tl) -> 
      T (f, List.map (rename ren) tl);;


let refresh t = 
  let ren = Renaming.make () 
  in rename ren t;;


let is_variable = function
    T _ -> false
  | V _ -> true;;


let to_variable = function
    T _ -> failwith "Term.to_variable: variable term expected"
  | V x -> x;;


let topsymbol = function
    V _ -> failwith "Term.topsymbol: called with variable"
  | T (s, _) -> s;;


let is_atom t = 
    if (is_variable t)
    then false 
    else 
      let s = (topsymbol t)
      in (Symbol.get_kind s) = PREDICATE ||
	 (Symbol.get_kind s) = SPLITTING;;
  

let rec check = function
    V x -> 
      Trace.check (Variable.check x)	
	"Term.check: variable error %s\n" (Variable.to_string x)
  | T (f, tl) -> 
      (Trace.check (Symbol.check f)  
 	 "Term.check: topsymbol error %s\n" (Symbol.to_string f))
      && (Trace.check ((get_arity f) = (List.length tl)) 
	    "Term.check: arity error %s\n" (Symbol.to_string f))
      && (for_all check tl);;


let subterms = function
    V _ -> []
  | T (_, tl) -> tl;;


let vars t = 
  (** [vars1 t vl] add the list of vars of [t] to [vl] *)
  let rec vars1 vl = function
      V x -> 
	if (List.mem x vl)
	then vl
	else x::vl
    | T (_, tl) -> 
	List.fold_left vars1 vl tl
  in vars1 [] t;;


let is_flat = function
    V _ -> false
  | T (s, tl) -> List.for_all is_variable tl;;


let is_linear t = 
  let vl = ref [] in
  let rec is_linear1 = function
      V x -> 
	if (List.mem x !vl)
	then false
	else begin (vl := x::!vl); true	end
    | T (_, tl) -> 
	List.for_all is_linear1 tl
  in is_linear1 t;;


let rec size = function
    V _ -> 1
  | T (_, tl) -> (List.fold_left (fun i t -> i + (size t)) 0 tl) + 1;;


let rec is_ground = function
    V _ -> false
  | T (_, tl) -> for_all is_ground tl;;


let rec occur x = function
    V y -> (x = y)
  | T (_, tl) -> exists (occur x) tl;;


let rec map_var f = function
    V x -> V (f x)
  | T (s, tl) -> T (s, List.map (map_var f) tl);;


let rec exists_var p = function
    V x -> (p x)
  | T (s, tl) -> List.exists (exists_var p) tl;;


let rec for_all_var p = function
    V x -> (p x)
  | T (s, tl) -> List.for_all (for_all_var p) tl;;


let equal s t = 
  let rec equal1 = function
      (V x, V y) -> Variable.equal x y
    | (T (f, sl), T (g, tl)) -> 
	assert (get_arity f = List.length sl);
	assert (get_arity g = List.length tl);
	(Symbol.equal f g) && (List.for_all equal1 (List.combine sl tl))
    | V _, T _
    | T _, V _ -> false 
  in equal1 (s, t);;
	  

let rec block = function
    V x as t -> t
  | T (s, tl) -> T ((Symbol.block s), (map block tl));;


let rec unblock = function
    V x as t -> t
  | T (f, tl) -> T ((Symbol.unblock f), (map unblock tl));;


let is_blocked = function
    V _ -> true
  | T (f, _) -> (Symbol.is_blocked f);;


