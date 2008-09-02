(** Module for function and predicate symbols *)

    
open Printf;;
open List;;
open Stream;;
open Trace;;
open Util;;
open Parsing;;


(** symbols internally stored on 32 bits integers,
    Starting from last bits:
    - 2 bits nots used
    - 2 bits: kind of symbol
    0x1 = FUNCTION
    0x2 = PREDICATE
    0x3 = SPLITTING
    - 4 bits : flags for other marks 
    0x0 = no mark
    0x1 = blocked
    0x2 = test predicate  
    - 8 bits: arity
    - rest: identifier *)
type symbol = int;;


let kind_mask  = 0x30000000;;
let kind_shift = 28;;
let flag_mask  = 0x0F000000;;
let flag_shift = 24;;
let arity_mask = 0x00FF0000;;
let arity_shift = 16;;
let index_mask = 0x0000FFFF;;
let index_shift = 0;;


type symbol_kind = 
    FUNCTION
  | PREDICATE
  | SPLITTING
;;


(** internal counter for the number of symbols *)
let cpt = ref 0;;


(** Internal association list to store symbol names (string). 
    For convenience, we add along with each string the 
    identifier [id] returned by {! Symbol.make}
    or {! Symbol.of_string} at the symbol registration.
    The [index] part of [id] must be the position in [symbol_list]. *)
let symbol_list = ref ([]: (string * symbol) list);;


let kind_of_int i = 
  assert ((1 <= i) && (i <= 3)); 
  (* debug "kind_of_int: unexpected kind value %i\n" i; *)
  match i with
      1 -> FUNCTION
    | 2 -> PREDICATE
    | 3 -> SPLITTING
    | _ -> raise (Internal_error 
		    (sprintf "kind_of_int: unexpected kind value %i\n" i));;


let int_of_kind = function
    FUNCTION -> 1
  | PREDICATE -> 2
  | SPLITTING -> 3;;

      
let string_of_kind = function
    FUNCTION -> "FUNCTION"
  | PREDICATE -> "PREDICATE"
  | SPLITTING -> "SPLITTING";;


let get_kind id =  
  kind_of_int ((id land kind_mask) lsr kind_shift);;


(************)
(*  Markers *)
(************)

(** symbol is blocked (for basic strategy). *)
let flag_block = 1;;

(** predicate symbol is a "test" predicate. *)
let flag_test = 2;;


let set_flag i id =  
  if (i < 1) || (i > 4)
  then 
    invalid_arg "Symbol.set_flag"
  else
    id lor (1 lsl (flag_shift+i-1));;


let unset_flag i id =  
  if (i < 1) || (i > 4)
  then 
    invalid_arg "Symbol.unset_flag"
  else
    id land (lnot (1 lsl (flag_shift+i-1)));;


let get_flag i id =  
  if (i < 1) || (i > 4)
  then 
    invalid_arg "Symbol.unset_flag"
  else
    let k = ((id land flag_mask) lsr flag_shift) land (1 lsl (i-1)) 
    in (k > 0);;


let block = set_flag flag_block;;


let unblock = unset_flag flag_block;;


let is_blocked = get_flag flag_block;;


let set_testpredicate = set_flag flag_test;;


let unset_testpredicate = unset_flag flag_test;;


let is_testpredicate = get_flag flag_test;;


let check id = 
  (* flags are compatible only with certain kinds *)
  ((not (is_blocked id)) || (get_kind id = FUNCTION))
  && ((not (is_testpredicate id)) || (get_kind id = PREDICATE));;

(** [get_index n]
    {b private}
    @return the index of the symbol of id [n] in the list of symbols *)
let get_index id = 
  id land index_mask;;


let is_registered id =
  (get_index id) < !cpt;;


let get_arity id = 
  (id land arity_mask) lsr arity_shift;;
    

let mem_symbol name = 
  List.mem_assoc name !symbol_list;;


let get_symbol name = 
  if (mem_symbol name)
  then List.assoc name !symbol_list
  else raise Not_found;;


let find_symbol name = 
  assoc name !symbol_list;;


let make name arity kind = 
  if arity > 255
  then failwith 
    (sprintf "Symbol.make: arity %i of symbol %s is too big." arity name)
  else if !cpt >= 65535
  then failwith "Symbol.make: too many symbols"
  (* symbol already registered *)
  else if (mem_symbol name)
  then 
    let id0 = (find_symbol name) in
    let arity0 = (get_arity id0) in
    let kind0 = (get_kind id0) in 
      if arity <> arity0
      then failwith
	(sprintf "Symbol.make: symbol %s already registered with a different arity %i"
	      name arity0)
      else if kind <> kind0
      then failwith
	(sprintf "Symbol.make: symbol %s already registered with a different kind %s" 
	   name (string_of_kind kind0))
	(* return id registered *)
      else id0
	(* new symbol, new id *)
  else 
    let id = 
      !cpt 
      lor (arity lsl arity_shift) 
      lor ((int_of_kind kind) lsl kind_shift) in 
      symbol_list := !symbol_list @ [ (name, id) ];
      incr cpt;
      (trace 4 "new symbol %s, arity %i, %s, index=%i\n" 
	 name arity (string_of_kind kind) (length !symbol_list));
      id;;


(** Register the reserved symbols *)
let cons_symbol = make "_cons" 2 FUNCTION;;

let bind_symbol = make "_bind" 2 FUNCTION;;

let empty_symbol = make "_null" 0 FUNCTION;;

let eq_symbol = make "=" 2 PREDICATE;;


(* Stop when non alphanumeric char encountered *)
let rec parse_symbol st = 
  (* location starts with this symbol *)
  restart_location ();
  if Util.is_empty st
  then raise (parse_error "symbol id expected")
  else 
    let c = (nextchar st) in match c with
	'a'..'w' -> 
	  ((String.make 1 c)^(parse_symbol_f1 st)), FUNCTION
      | '0'..'9' -> 
	  ((String.make 1 c)^(parse_symbol_i st)), FUNCTION
      | 'x'..'z' ->
	  ((String.make 1 c)^(parse_symbol_f2 st)), FUNCTION
      | 'A'..'Z' -> 
	  ((String.make 1 c)^(parse_symbol_p st)), PREDICATE
      | _ -> raise (parse_error "symbol id expected")
and parse_symbola ar st = 
  let (s, k) = (parse_symbol st)
  in make s ar k
(** parse function symbol 
    starting with a letter other than 'x', 'y', 'z' *)
and parse_symbol_f1 st = 
  if Util.is_empty st
  then ""
  else 
    let c = (surepeek st) in match c with
	'a'..'z' 
      | 'A'..'Z' 
      | '_' -> (skip st); (String.make 1 c)^(parse_symbol_f1 st)
      | '0'..'9' -> (skip st); (String.make 1 c)^(parse_symbol_i st)
      | _ ->  ""
(** parse function symbol 
    starting with either 'x', 'y' or 'z' *)
and parse_symbol_f2 st = 
  if Util.is_empty st
  then raise (parse_error "symbol id expected (variable found)")
  else 
    let c = (surepeek st) in match c with
	'a'..'z' 
      | 'A'..'Z' 
      |  '_' -> (skip st); (String.make 1 c)^(parse_symbol_f1 st)
      | '0'..'9' -> 
	  raise (parse_error "symbol id expected (variable found)")
      | _ ->  ""
(** parse predicate symbol *)
and parse_symbol_p st = 
  if Util.is_empty st
  then ""
  else 
    let c = (surepeek st) in match c with
	'a'..'z' 
      | 'A'..'Z' 
      | '_' -> (skip st); (String.make 1 c)^(parse_symbol_p st)
      | '0'..'9' -> (skip st); (String.make 1 c)^(parse_symbol_i st)
      | _ ->  ""
(** parse integer terminating a symbol
    @return string parsed *)
and parse_symbol_i st = 
  if Util.is_empty st
  then ""
  else 
    let c = (surepeek st) in match c with
	'0'..'9' -> (skip st); (String.make 1 c)^(parse_symbol_i st)
      | _ ->  "";;


let of_string name arity = 
  let st = (Stream.of_string name)
  in parse_symbola arity st;;
   

let nb_symbols () =
  !cpt;;


let get_name id = 
  assert (check id);
  let index = (get_index id) in
    (* DEBUG *)
    if index > !cpt
    then       
      raise (Internal_error (sprintf "mem_symbol: symbol id %X out of bounds." id))
    (* END DEBUG *)
    else let (name, s) = (nth !symbol_list index) in
      (* DEBUG *)
      if (get_index s) <> index 
      then raise (Internal_error 
		    (sprintf 
		       "get_name: symbol %s stored with index %i at place %i." 
		       name (get_index s) index))
      else if (get_arity s) <> (get_arity id)
      then raise (Internal_error 
		    (sprintf 
		       "get_name: symbol %s, arity of id %i differs from stored arity %i." 
		       name (get_arity id) (get_arity s)))
      else if (get_kind s) <> (get_kind id)
      then raise (Internal_error 
		    (sprintf 
		       "get_name: symbol %s, kind of id %s differs from stored kind %s." 
		       name (string_of_kind (get_kind id)) 
		       (string_of_kind (get_kind s))))
	(* END DEBUG *)
      else name;;


let equal id1 id2 = 
  assert (((get_index id1) <> (get_index id2)) || 
	    ((get_kind id1) = (get_kind id2)));
  assert (((get_index id1) <> (get_index id2)) || 
	    ((get_arity id1) = (get_arity id2)));
  assert (((get_index id1) <> (get_index id2)) || 
	    ((get_name id1) = (get_name id2)));    
  (get_index id1) = (get_index id2);;


let rec member s = function
    [] -> false
  | a::l when equal a s -> true
  | _::l -> member s l;;


(** Precedence on symbols is such that:
  [FUNCTION < SPLITTING < PREDICATE]
  and in each category, the symbols
  are (linearly) ordered, increasingly,
  by their order of declaration with {! make_symbol}. *)
let ( << ) id1 id2 = 
  (* FUNCTION << SPLITTING << PREDICATE *)
  let smaller_kind = function
      FUNCTION -> 
	(function
	     SPLITTING -> true
	   | PREDICATE -> true
	   | FUNCTION -> false)
    | SPLITTING -> 
	(function
             PREDICATE -> true
	   | SPLITTING -> false 
	   | FUNCTION -> false)
    | PREDICATE -> (fun x -> false) in
  let k1 = (get_kind id1) in
  let k2 = (get_kind id2) in
    if k1 = k2
    then (get_index id1) < (get_index id2)
    else (smaller_kind k1 k2);;


let kind_to_string = function
    FUNCTION -> "FUNCTION"
  | PREDICATE -> "PREDICATE"
  | SPLITTING -> "SPLITTING";;


let to_string id =
  let flag_suffix = ref "" in
    begin
      if (get_flag 1 id)
      then flag_suffix := !flag_suffix^".1";
      if (get_flag 2 id)
      then flag_suffix := !flag_suffix^".2";
      if (get_flag 3 id)
      then flag_suffix := !flag_suffix^".3";
      if (get_flag 4 id)
      then flag_suffix := !flag_suffix^".4";
      (get_name id)^(!flag_suffix)
    end;;


let dump id =
  printf "%X" id;;


let dump_symbols () = 
  printf "%i symbols\n" (length !symbol_list);
  printf "cpt = %i\n" !cpt;  
  printf "%-6s  %-5s %-9s %-5s %-2s %-2s %-2s %-2s\n" 
    "symbol" "arity" "kind" "id" "f1" "f2" "f3" "f4";
  List.iter
    (fun (name, id) -> 
      printf "%-6s  %-5i %-9s %-5i %-2i %-2i %-2i %-2i\n" 
	name (get_arity id) (string_of_kind (get_kind id)) (get_index id) 
	(if (get_flag 1 id) then 1 else 0)
	(if (get_flag 2 id) then 1 else 0)
	(if (get_flag 3 id) then 1 else 0)
	(if (get_flag 4 id) then 1 else 0))
    !symbol_list;;
