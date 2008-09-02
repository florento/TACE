(** implementation with lists 
    with indices in reverse order *)

(* TODO code with hashtable *)


open List;;
open Printf;;
open Format;;

open Trace;;
open Util;;
open Symbol;;
open Variable;;
open Term;;
open Atom;;
open Trs;;
open Clausetype;;
open Clause;;
open Instruction;;
open Subsumption;;


(**********************************************)
(**                 Creation                  *)
(**********************************************)

(** internal type for a clause with the instruction that 
    made it and a flag telling whether this clause was deleted *)
type cell = 
    { 
      c_clause : clause;
      c_instr : instruction; 
      c_deleted : bool;
    };;


type clauseset = {
  (* the set of clauses with for each clause the instruction that 
     made it and a flag telling whether this clause was deleted *)
  mutable cs_clauses : cell list;
  (* positive equational clauses are put apart in this trs *)
  mutable cs_trs : trs;
  (* sum of the respective sizes of all the clauses of the clauseset and trs *)
  mutable cs_size : int;
  (* the number of deleted clauses (free positions) *)
  mutable cs_deleted : int;
  (* index, TODO *)
  (* mutable cs_index : index *)
};;


let make r = 
  debug "Clauseset.make\n";
  {
    cs_clauses = [];         
    cs_trs = r;
    cs_size = 0;
    cs_deleted = 0;
  };;


let makenew () = 
  make (Trs.make ());;


let set_trs r cs = 
  if not (Trs.is_empty cs.cs_trs)
  then warning "Clauseset.set_trs: trs is already set\n";
  cs.cs_trs <- r;;


(**********************************************)
(**            Counters, stats                *)
(**********************************************)


let length cs = 
  List.length cs.cs_clauses;;


let size cs = 
  cs.cs_size;;

let deleted cs =
  cs.cs_deleted;;


(**********************************************)
(**                   Access                  *)
(**********************************************)


let get_trs cs =
  cs.cs_trs;;


let mem i cs = 
  i <= (length cs);;


let get_cell i cs = 
  let cl = cs.cs_clauses
  in List.nth cl ((List.length cl)-i-1);;


let get i cs = 
  let c = get_cell i cs
  in (c.c_clause, c.c_instr);;


let is_deleted i cs = 
  assert (debug_b "Clauseset.is_deleted %i of %i = %s\n" 
	    i (length cs) (string_of_bool (get_cell i cs).c_deleted));
  let c = (get_cell i cs) 
  in c.c_deleted;;


let get_clause i cs = 
  assert (debug_b "Clauseset.get_clause %i of %i\n" i (length cs));
  (get_cell i cs).c_clause;;


let get_clause2 i cs = 
  assert (debug_b "Clauseset.get_clause2 %i of %i\n" i (length cs));
  let c = (get_cell i cs) 
  in (c.c_clause, c.c_deleted);;


let get_instruction i cs = 
  assert (debug_b "Clauseset.get_instruction %i of %i\n" i (length cs));
  (get_cell i cs).c_instr;;
  

(**********************************************)
(**       Modification and saturation         *)
(**********************************************)


let delete_all p cs = 
  let rec delete_all1 keep = function
      [] -> List.rev keep	
    | c::l -> 
	if c.c_deleted
	then 
	  delete_all1 (c::keep) l
	else if (p c.c_clause)
	then begin 
	  cs.cs_deleted <- cs.cs_deleted + 1; 
	  let c1 = 
	    { c_deleted = true;
	       (* Clause is freed from memory *)	       
	      c_clause = Clause.empty ();
	      c_instr = c.c_instr } 
	  in delete_all1 (c1::keep) l
	end
	else 
	  delete_all1 (c::keep) l
  in cs.cs_clauses <- (delete_all1 [] cs.cs_clauses);;
    

let add c i cs =   
  assert (debug_b "Clauseset.add %s  (%s)\n" 
	    (Clause.to_stringt c)
	    (Instruction.to_string i));
  let idx = 
    if (Clause.is_equation c)
    then 
      begin
	cs.cs_trs <- Trs.add (Clause.to_equation c) (cs.cs_trs);
	(Trs.length cs.cs_trs) - 1
      end      
    else 
      begin
	cs.cs_clauses <- 
	  { c_deleted = false;
	    c_clause = c;
	    c_instr = i }::cs.cs_clauses;
	(List.length cs.cs_clauses) - 1
      end
  in 
    cs.cs_size <- cs.cs_size + (Clause.size c);
    assert (debug_b  "Clauseset.add %s  (%s) index: %i\n" 
	      (Clause.to_stringt c)
	      (Instruction.to_string i)
	      idx);
    idx;;


let exists p cs = 
  List.exists (fun c -> (not c.c_deleted) && (p c.c_clause)) cs.cs_clauses;;

     
let map f cs = 
  let rec map1 accu = function
      [] -> rev accu
    | c::l -> 
	if c.c_deleted
	then map1 accu l
	else map1 ((f c.c_clause)::accu) l
  in  map1 [] cs.cs_clauses;;


(** CAUTION: specific to the implementation with lists
    in reverse order. *)
let mapi_opt f cs = 
  let rec mapi_opt1 i accu = function
      [] -> rev accu
    | c::l -> 
	if c.c_deleted
	then mapi_opt1 (i-1) accu l
	else match (f c.c_clause i) with
	    None -> 
	      mapi_opt1 (i-1) accu l
	  | Some x -> 
	      mapi_opt1 (i-1) (x::accu) l
  in mapi_opt1 ((length cs)-1) [] cs.cs_clauses;;


let mapi f = 
  mapi_opt (fun c i -> Some (f c i));;


let flatmap f cs = 
  let rec flatmap1 accu = function
      [] -> accu
    | c::l -> 
	if c.c_deleted
	then flatmap1 accu l
	else let l1 = f c.c_clause in
	  flatmap1 (l1@accu) l
  in flatmap1 [] cs.cs_clauses;;


(** CAUTION: specific to the implementation with lists in reverse order. *)
let flatmapi f cs = 
  let rec flatmapi1 i accu = function
      [] -> accu
    | c::l -> 
	if c.c_deleted
	then flatmapi1 (i-1) accu l
	else let l1 = f c.c_clause i in
	  flatmapi1 (i-1) (l1@accu) l
  in flatmapi1 ((length cs)-1) [] cs.cs_clauses;;



(**********************************************)
(**     Backward and Forward Subsumption      *)
(**********************************************)
(** brute force implementation - assume good filters *)
(** TODO with index in clauseset for optimization ?  *)


let forward_subsume cs c = 
  debug "Subsumption.forward_subsume %s\n" (Clause.to_string c);
  exists (fun c1 -> subsume c1 c) cs;;


let backward_subsume c cs =
  debug "Subsumption.backward_subsume %s\n" (Clause.to_string c);
  delete_all (fun c1 -> subsume c c1) cs;;



(**********************************************)
(**            Output and debug               *)
(**********************************************)


let dump cs = 
  let n = (length cs) in
    open_box 8; 
    debug "Dump clauseset (";
    print_int n;
    print_string " clauses):";
    print_newline ();
    for i = 0 to (n-1) do
      let c = get_cell i cs in
	open_box 0; 
	printf "%4i " i;
	print_char '[';
	print_string (Clausetype.to_string (Clause.get_type c.c_clause));
	print_char ']';
	print_space ();
	print_string (Clause.to_string c.c_clause);
	print_space ();
	print_char ':';
	print_space ();	
	print_string (Instruction.to_string c.c_instr);
	print_space ();
	printf "%s" (if c.c_deleted then "<DELETED> " else "");
	print_newline ();
    done;
    close_box ();;
