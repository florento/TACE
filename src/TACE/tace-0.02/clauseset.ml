(** implementation with lists 
    with indices in reverse order *)



open List;;
open Hashtbl;;
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
      mutable c_clause : clause;
      mutable c_instr : instruction; 
      mutable c_deleted : bool;
    };;


type clauseset = {
  (* the set of clauses with for each clause the instruction that 
     made it and a flag telling whether this clause was deleted *)
  mutable cs_clauses : (int, cell) Hashtbl.t;
  (* positive equational clauses are put apart in this trs *)
  mutable cs_trs : trs;
  (* counter of the position of the next clause added *)
  mutable cs_last : int;
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
    cs_clauses = Hashtbl.create 2048;         
    cs_trs = r;
    cs_last = 0;
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
  Hashtbl.length cs.cs_clauses;;


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
  Hashtbl.mem cs.cs_clauses i;;


let get_cell i cs = 
  assert (mem i cs);
  Hashtbl.find cs.cs_clauses i;;


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
	let pos = cs.cs_last in
	let ce = { c_deleted = false;
		   c_clause = c;
		   c_instr = i } in
	  cs.cs_last <- pos + 1;
	  Hashtbl.add cs.cs_clauses pos ce;
	  pos
      end
  in 
    cs.cs_size <- cs.cs_size + (Clause.size c);
    assert (debug_b  "Clauseset.add %s  (%s) index: %i\n" 
	      (Clause.to_stringt c)
	      (Instruction.to_string i)
	      idx);
    idx;;


let cell_delete c = 
  (* if c.c_deleted
  then warning "Clauseset.cell_delete %i: already deleted\n" i; *)
  c.c_deleted <- true;
  (* Clause is freed from memory, 
     instruction is kept for reconstruction *)	       
  c.c_clause <- Clause.empty ();;


let delete_all p cs = 
  Hashtbl.iter 
    (fun i c -> if (not c.c_deleted) && (p c.c_clause) then cell_delete c)
    cs.cs_clauses;;
    

let delete i cs = 
  let c = get_cell i cs in
    if c.c_deleted
    then warning "Clauseset.delete %i: already deleted\n" i
    else Hashtbl.replace cs.cs_clauses i
      { 	 
	c_clause = Clause.empty ();
	c_instr = c.c_instr;
	c_deleted = true;
      };;


exception Clauseset_found;;


let exists p cs = 
  try 
    Hashtbl.iter (fun i c -> 
		    if ((not c.c_deleted) && (p c.c_clause))
		    then raise Clauseset_found) 
      cs.cs_clauses;
    false
  with 
      Clauseset_found -> true;; 

(*   Hashtbl.fold (fun i c b -> ((not c.c_deleted) && (p c.c_clause)) || b) cs.cs_clauses false;; *)


     
let mapi f cs = 
  Hashtbl.fold 
    (fun i c l -> 
       if c.c_deleted
       then l
       else (f c.c_clause i)::l)
    cs.cs_clauses [];;


let map f cs = 
  let f1 c i = f c
  in mapi f1 cs;;


let flatmapi f cs = 
  Hashtbl.fold 
    (fun i c l -> 
       if c.c_deleted
       then l
       else let cl = f c.c_clause i in 
	 if cl = [] 
	 then l
	 else rev_append cl l)
    cs.cs_clauses [];;


let flatmap f cs = 
  let f1 c i = f c 
  in flatmapi f1 cs;;
  


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

let cell_dump i c = 
  open_box 0; 
  printf "%4i " i;
  print_char '[';
  print_string (Clausetype.to_string (Clause.get_type c.c_clause));
  print_char ']';
  print_space ();
  print_string (Clause.to_string c.c_clause);
  print_space ();
  print_char '<';
  print_string (Instruction.to_string c.c_instr);
  print_char '>';
  print_space ();
  printf "%s" (if c.c_deleted then "<DELETED> " else "");
  print_newline ();;
  


let dump cs = 
  open_box 8; 
  debug "Dump clauseset (";
  print_int (length cs);
  print_string " clauses):";
  print_newline ();
  Hashtbl.iter cell_dump cs.cs_clauses;
  close_box ();;
