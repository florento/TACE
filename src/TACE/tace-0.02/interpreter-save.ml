(** TDC remove split dans les exec *)

open List;;
open Printf;;

open Trace;;
open Util;;
open Chrono;;
open Instruction;;
open Clause;;
open Clausetype;;
open Clauseset;;
open Esplitting;;
open Narrowing;;
open Lparamodulation;;
open Solving;;
open Rparamodulation;;
open Resolution;;
open Esplitting;;
open Tautology;;
open Elim;;


type monitor = {
  mutable interp_eager_time : chrono;
  (** chronometer of the time spent in application of eager inferences. *)
}


let monitor =     
  {
    interp_eager_time = Chrono.make "ch_interpreter_eager";
  };;


(******************************)
(* Non-eager inferences       *)
(******************************)


let debug_eager msg cl = 
  if !debug_mode
  then begin
    debug "Interpreter.eager: %i clauses %s\n"
      (List.length cl) msg;
    dump_list 
      (fun c -> debug "  %s\n" (Clause.to_stringt c)) cl;
  end;;  



let exec_tautology cs cl = 
  List.filter (fun c -> not (tautology c)) cl;;


let exec_elim cs cl = 
  List.filter (fun c -> not (tautology c)) cl;;



(** eager application of splitting to a new clause.
    - epsilon splitting
    - TODO: non-ground splitting (optimisation). 
    @deprecated *)
let exec_esplit cs c = 
  Esplitting.esplit c;;


let eager cs cl0 = 
  assert (debug_b "Interpreter.eager %s\n"
	    (Util.list_to_string (fun c -> Clause.to_stringt c) cl0));
  Chrono.start monitor.interp_eager_time;
  let cl1 = 
    (debug_eager "returned" cl0);
    List.filter (fun c -> not (tautology c)) cl0 in
  let cl2 = 
    (debug_eager "after tautology deletion" cl1);
    List.map (fun c -> elim c) cl1 in
  let cl3 = 
    (debug_eager "after trivial lit. elimination" cl2);
    List.flatten (List.map (fun c -> esplit c) cl2) in
  let cl4 = 
    (debug_eager "after e-splitting" cl3);
    List.filter (fun c -> not (tautology c)) cl3
  in 
    Chrono.stop monitor.interp_eager_time;    
    (debug_eager "returned" cl4); cl4




(******************************)
(* Non-eager inferences       *)
(******************************)

let exec_nil_c cs = 
  [];;


let exec_nil cs = 
  exec_nil_c cs;;


let exec_lparamodulation_c cs c = 
  eager cs (Lparamodulation.lparamodulation (get_trs cs) c);;


let exec_lparamodulation cs c i = 
  Util.mapi (fun c1 j -> (c1, LPA (i, j))) (exec_lparamodulation_c cs c);;


let exec_solve_c cs c = 
  eager cs (solve_all (get_trs cs) c);;


let exec_solve cs c i = 
  Util.mapi (fun c1 j -> (c1, SOL (i, j))) (exec_solve_c cs c);;


let exec_rparamodulation_c cs c = 
  eager cs (rparamodulation (get_trs cs) c);;


let exec_rparamodulation cs c i = 
  Util.mapi (fun c1 j -> (c1, RPA (i, j))) (exec_rparamodulation_c cs c);;


let exec_new_c cs c = 
  eager cs [c];;
  

let exec_new cs c = 
  Util.mapi (fun c1 j -> (c1, NEW (c, j))) (exec_new_c cs c);;


let exec_resolution_c cs c1 c2 = 
  eager cs (resolution c1 c2);;    


let exec_resolution cs c1 i1 c2 i2 = 
  Util.mapi (fun c j -> (c, RES (i1, i2, j)))
    (exec_resolution_c cs c1 c2);;


(** [exec_resolutions_c1 cs c1]
    @return a list of triples [(c, dir, i2)] where
    - [c] is a clause obtained by resolution between [c1]
    and the clause [c2] of index [i2] in [cs]
    - dir is 0 if it is a resolution step of [c1] into [c2]
    - dir is 1 if it is a resolution step of [c2] into [c1]. *)
let exec_resolutions_c1 cs c1 = 
  assert (debug_b "Interpreter.exec_resolutions_c %s\n" 
	    (Clause.to_stringt c1));
  (Clauseset.flatmapi 
     (fun c2 i2 -> List.map (fun c -> (c, 0, i2)) (resolution c1 c2))
     cs)
  @ 
  (Clauseset.flatmapi 
     (fun c2 i2 -> List.map (fun c -> (c, 1, i2)) (resolution c2 c1))
     cs);;


let exec_resolutions_c cs c1 = 
  List.map (fun (c, _, _) -> c) (exec_resolutions_c1 cs c1);;


(** [split l]
    @parameter [l] a list of triples  [(c, dir, i)]
    as returned by {! exec_resolutions_c1}
    @return an association list with pairs [((d, i), cl)] 
    where [cl] is the list of clauses [c] such that
    [(c, d, i)] is in [l]. *)    
let split cdil = 
  let rec split1 accu = function
      [] -> accu
    | (c, d, i)::l as ll -> 
	if mem_assoc (d, i) accu
	then split1 accu l
	else 
	  let part = 
	    ((d, i),
	     List.map 
	       (fun (c1, d1, i1) -> c1)
	       (List.filter (fun (c2, d2, i2) -> d2 = d && i2 = i) ll)) 
	  in split1 (part::accu) l 
  in split1 [] cdil;; 
      
  
let exec_resolutions cs c1 i1 = 
  let cdil = exec_resolutions_c1 cs c1 in
  let sl = split cdil in
  let sle = List.map (fun (p, cl) -> (p, eager cs cl)) sl in
  let cil = 
    List.map 
      (fun ((d, i2), cl) -> 
	 (Util.mapi (fun c j -> 
		       if d = 0 
		       then (c, RES (i1, i2, j))
		       else (c, RES (i2, i1, j))) cl)) sle
  in List.flatten cil;;



(******************************)
(* Instructions execution     *)
(******************************)

let check_get i cs = 
  assert (mem i cs);
  if is_deleted i cs 
  then begin
    warning "exec: clause %i has been deleted\n" i;
    None
  end
  else 
    Some (get_clause i cs);;
  

let exec1 get cs ins = 
  match ins with
      NIL -> 
	[]
    | LPA (i, j) -> 
	(match (get i cs) with
	     None -> []
	   | Some c -> 
	       let cl = exec_lparamodulation_c cs c in
		 assert (List.length cl > j);
		 [List.nth cl j])
    | SOL (i, j) -> 
	(match (get i cs) with
	     None -> []
	   | Some c -> 
	       let cl = exec_solve_c cs c in
		 assert (List.length cl > j);
		 [List.nth cl j])
    | RPA (i, j) ->
	(match (get i cs) with
	     None -> []
	   | Some c -> 
	       let cl = exec_rparamodulation_c cs c in
		 assert (List.length cl > j);
		 [List.nth cl j])
    | NEW (c, j) -> 
	[List.nth (exec_new_c cs c) j]
    | RES (i1, i2, j) -> 
	(match (get i1 cs) with
	     None -> []
	   | Some c1 -> 
	       (match (get i2 cs) with
		    None -> []
		  | Some c2 -> 
		      let cl = exec_resolution_c cs c1 c2 in
			debug ">>> Interpreter RES %i %i %i\n" i1 i2 j;		
			debug ">>> length exec_res %i\n" (List.length cl);
			assert (List.length cl > j);
			[List.nth cl j]));;


let exec cs ins =   
  assert (debug_b "Interpreter.exec %s\n" (Instruction.to_string ins));
  exec1 check_get cs ins;;


(******************************)
(* Recovering deleted clauses *)
(******************************)


let rec recover i cs = 
  assert (debug_b "Interpreter.recover %i (%s)\n" 
	    i (Instruction.to_string (get_instruction i cs)));
  assert (mem i cs);
  if is_deleted i cs 
  (* we recover the clause by execution of the instruction *)
  then
    let ins = (get_instruction i cs) in
      if ins = NIL
      then begin
	warning "Interpreter.recover %i, intruction NIL\n" i;
	Clause.empty ()
      end
      else begin 
	assert (debug_b "Interpreter.recover INSTR: %s\n" 
		  (Instruction.to_string ins));
	let cl = exec1 get_recover cs ins in 
	  assert ((List.length cl) = 1); 
	  hd cl
      end
  (* the clause is not deleted, just get it in [cs] *)
  else 
    get_clause i cs
and get_recover i cs = 
  Some (recover i cs);;
