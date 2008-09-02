(** Variables manipulation *)

open List;;
open Printf;;
open Stream;;
open Trace;;
open Util;;
open Parsing;;


let index_mask = 0x00FFFFFF;;
let flag_mask = 0x10000000;;
let flag_shift = 28;;
let color_mask = 0x0F000000;;
let color_shift = 24;;


(** variables are internally stored on 32 bits integers,
    Starting from last bits:
    - 3 bits nots used
    - 1 bit: flag, 
      1 if the var was created by {! Variable.of_string},
      0 if the var was created by {! Variable.make}    
    - 4 bits: color
    - rest: variable index *)
type variable = int;;


let var0 = 0;;


(** Internal association list to store names (string)
    of var registered with {! Variable.of_string},
    along with their respective variable index 
    (as returned by {! Variable.of_string}). *)
let var_list = ref ([]: (string * variable) list);;


(** internals counter for the variable indexes
    returned by {! Variable.of_string}.
    The first variable registered with 
    {! Variable.of_string} has index [1]
    (index [0] is reserved). 
    Hence, the list of ids of registered variables is always [1..!cpt_var-1]. *)
let cpt_var = ref 1;;


let make i =
  if i >= 0XFFFFFF
  then failwith "make_var: index too big"
  else i land index_mask;;


let set_flag v =  
  v lor (1 lsl flag_shift);;

let unset_flag v =  
  let mask = lnot (1 lsl flag_shift) 
  in v land mask;;


let get_flag v =  
  let k = (v land flag_mask) lsr flag_shift in
    assert (k = 0 || k = 1); k;;


let new_var () = 
  if !cpt_var >= 0XFFFFFF
  then failwith "Too many variables."
  else let v = !cpt_var in
    incr cpt_var; 
    v lor flag_mask;;


(*
let get_index s = 
  List.assoc s !var_list;;
*)

(** {b private}
    Same as {! Variable.of_string} without 
    parsing of the passed string.

    [register_var s]
    @return a variable id uniquely associated to the given string [s].
    If the string [s] is bound in [var_list] 
    then return the id associated to [s] in [var_list],
    otherwise return a new id returned by [new_var ()]
    and update [var_list].   *)
let register_var s = 
  if (mem_assoc s !var_list)
  then (assoc s !var_list)
  else 
    let v = new_var () in  
    var_list := (s, v)::!var_list;
    v;;


let rec parse_variable st = 
  (* location starts with this id *)
  restart_location ();
  if Util.is_empty st
  then raise (parse_error "variable id expected")
  else 
    let c = (nextchar st) in match c with
	'x' | 'y' | 'z' -> 
	  register_var ((String.make 1 c)^(parse_variable_i st))
      | _ ->  raise (parse_error "variable id expected")
(** parse integer terminating a variable *)
and parse_variable_i st = 
  if (Util.is_empty st)
  then ""
  else 
    let c = (surepeek st) in match c with
	'0'..'9' -> (skip st); (String.make 1 c)^(parse_variable_i st)
      | _ -> "";;


let of_string s = 
  let st = Stream.of_string s
  in parse_variable st;;


(** [get_index v]
    {b private}
    @return the index of the variable [v] *)
let get_index v = 
  v land index_mask;;


let set_color i v =  
  v lor (i lsl color_shift);;


let decolor v =  
  v land (lnot color_mask);;


let get_color v =  
  let k = (v land color_mask) lsr color_shift in
    assert (k >= 0 && k <= 15);
    k;;

    
let equal v1 v2 = 
  v1 = v2;;
(*  get_index v1 = get_index v2;; *)


let rec pairwise_distinct = function
    [] -> true
  | i::l ->
      (not (List.memq i l)) && (pairwise_distinct l);;


(** stub, no check necessary *)
let check v = 
  true;;


let to_string v = 
  let i = (get_index v) in
  let flag = (get_flag v) in
  let c = (get_color v) in
  let sc = 
    if (c = 0)
    then ""
    else sprintf ".%i" c in
  let name = 
    if (flag = 1)
    then 
      if i >= !cpt_var
      then failwith 
	(sprintf "Variable.to_string: unregistered variable %i." i)
      else 
	begin
	  assert (Util.imem_assoc (decolor v) !var_list);
	  Util.iassoc (decolor v) !var_list
	end
    else (sprintf "v%i" i) in
    assert ((i >= 0) && (i <= 16777215));
    assert ((flag = 0) || (flag = 1));
    assert ((c >= 0) && (c <= 15));
    name^sc;;


let dump v = 
  printf "0X%08X" v;;


let dump_vars () = 
  printf "%i variables registered\n" (length !var_list);
  printf "cpt = %i\n" !cpt_var;  
  printf "%-8s %s\n" "id" "name";
  List.iter
    (fun (name, id) -> printf "%08X %s\n" id name)
    !var_list;;
