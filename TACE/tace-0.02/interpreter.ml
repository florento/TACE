(** TDC remove split dans les exec *)

open List;;
open Printf;;

open Trace;;
open Util;;
open Chrono;;
open Symbol;;
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
open Subsumption;;
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


let debug_eager msg cil = 
  if !debug_mode     
  then  
    begin
      debug "Interpreter.eager: %i clauses %s\n" (List.length cil) msg;
      List.iter 
	(fun (c, i) -> debug "... %s <%s>\n" 
	   (Clause.to_stringt c) (Instruction.to_string i))
	cil;
    end;;  


let exec_tautology cs cil = 
  List.filter (fun (c, _) -> not (tautology c)) cil;;


let exec_elim cs cil = 
  List.map (fun (c, i) -> (elim c, i)) cil;;


(** [exec_subsume cs cil]
    @return the list of clauses (with instructions) of [cil]
    not subsumed by another clause of [cil]. *)
let exec_subsume cs cil = 
  let rec exec_subsume1 accu = function
      [] -> List.rev accu
    | ci::l -> 
	let c = fst ci in
	  if List.exists (fun ci2 -> subsume (fst ci2) c) l
	  then exec_subsume1 accu l
	  else exec_subsume1 (ci::accu) l in 
    if List.length cil > 1
    then exec_subsume1 [] cil
    else cil;;


let clause_get_pos_splitting c = 
  let psl = get_splittings POS c in
  let a = assert (List.length psl = 1); hd psl in
  let q = Atom.get_predicate a in
    assert (get_kind q = SPLITTING);
    assert (get_arity q = 0);
    q;;


(** eager application of splitting to a new clause.
    - epsilon splitting
    - TODO: non-ground splitting (optimisation). *)
let exec_esplit cs c i = 
  assert (debug_b "Interpreter.exec_esplit %s\n" (Instruction.to_string i));
  let cl = Esplitting.esplit c in
    if cl = []
    (* not splitted *)
    then 
      [ (c, i) ]
    (* splitted *)
    else 
      let c1 = hd cl in
      let ci = (c1, i) in
      let cl1 = tl cl in
      let cil = List.map (fun c2 ->  (c2, ESP (clause_get_pos_splitting c2))) cl1
      in ci::cil;;


let eager cs cil0 = 
  assert (debug_b "Interpreter.eager %s\n"
	    (Util.list_to_string 
	       (fun (c, i) -> Clause.to_stringt c) 
	       cil0));
  if cil0 = []
  then []
  else begin
    Chrono.start monitor.interp_eager_time;
    let cil1 = 
      (debug_eager "initialy" cil0);
      exec_tautology cs cil0 in
    let cil2 = 
      (debug_eager "after tautology deletion" cil1);
      exec_elim cs cil1 in
    let cil3 = 
      (debug_eager "after trivial lit. elimination" cil2);
      exec_subsume cs cil2 in
    let cil4 = 
      (debug_eager "after elimination of subsumed" cil3);
      List.flatten (List.map (fun (c, i) -> exec_esplit cs c i) cil3) in
    let cil5 = 
      (debug_eager "after e-splitting" cil4);
      exec_tautology cs cil4
    in 
      Chrono.stop monitor.interp_eager_time;    
      (debug_eager "returned" cil5); cil5
  end;;


(******************************)
(* Non-eager inferences       *)
(******************************)


let exec_nil cs = 
  [];;


let exec_lparamodulation cs c i = 
  assert (debug_b "Interpreter.exec_lparamodulation %i\n" i);
  let cl = Lparamodulation.lparamodulation (get_trs cs) c in
  let cil = Util.mapi (fun c1 j -> (c1, LPA (i, j))) cl
  in eager cs cil;;


let exec_solve cs c i = 
  assert (debug_b "Interpreter.exec_solve %i\n" i);
  let cl = solve_all (get_trs cs) c in
  let cil =  Util.mapi (fun c1 j -> (c1, SOL (i, j))) cl 
  in eager cs cil;;


let exec_rparamodulation cs c i = 
  assert (debug_b "Interpreter.exec_rparamodulation %i\n" i);
  let cl = rparamodulation (get_trs cs) c in
  let cil = Util.mapi (fun c1 j -> (c1, RPA (i, j))) cl
  in eager cs cil;;


let exec_new cs c = 
  assert (debug_b "Interpreter.exec_new %s\n" (Clause.to_string c));
  let cil = [(c, NEW c)]
  in eager cs cil;;


let exec_resolution cs c1 i1 c2 i2 = 
  assert (debug_b "Interpreter.exec_resolution %i %i\n" i1 i2);
  let cl = resolution c1 c2 in
  let cil = Util.mapi (fun c j -> (c, RES (i1, i2, j))) cl
  in eager cs cil;;


(** [exec_resolutions_c1 cs c1]
    @return a list of triples [(c, dir, i2)] where
    - [c] is a clause obtained by resolution between [c1]
    and the clause [c2] of index [i2] in [cs]
    - dir is 0 if it is a resolution step of [c1] into [c2]
    - dir is 1 if it is a resolution step of [c2] into [c1]. *)
let exec_resolutions_c1 cs c1 = 
  (Clauseset.flatmapi 
     (fun c2 i2 -> List.map (fun c -> (c, 0, i2)) (resolution c1 c2))
     cs)
  @ 
  (Clauseset.flatmapi 
     (fun c2 i2 -> List.map (fun c -> (c, 1, i2)) (resolution c2 c1))
     cs);;


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
  assert (debug_b "Interpreter.exec_resolutions %i\n" i1);
  let cdil = exec_resolutions_c1 cs c1 in
  let sl = split cdil in
  let cil = 
    List.map 
      (fun ((d, i2), cl) -> 
	 (Util.mapi (fun c j -> 
		       if d = 0 
		       then (c, RES (i1, i2, j))
		       else (c, RES (i2, i1, j))) cl)) sl
  in eager cs (List.flatten cil);;



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
  

let find cs ins cil = 
  try 
    fst (List.find (fun (_, ins1) -> (Instruction.equal ins1 ins)) cil)
  with 
      Not_found -> 
	error "Interpreter.find: %s not found\n" (Instruction.to_string ins);
	error "... cil = \n";
	List.iter 
	  (fun (c, i) -> error "... %s <%s>\n" 
	     (Clause.to_stringt c) (Instruction.to_string i))
	  cil;
	Clause.empty ();;


let exec1 get cs ins = 
  match ins with
      NIL -> 
	[]
    | LPA (i, j) -> 
	(match (get i cs) with
	     None -> []
	   | Some c -> 
	       let cil = exec_lparamodulation cs c i
	       in [find cs ins cil])
    | SOL (i, j) -> 
	(match (get i cs) with
	     None -> []
	   | Some c -> 
	       let cil = exec_solve cs c i
	       in [find cs ins cil])
    | RPA (i, j) ->
	(match (get i cs) with
	     None -> []
	   | Some c -> 
	       let cil = exec_rparamodulation cs c i
	       in [find cs ins cil])
    | NEW c -> 
	let cil = exec_new cs c 
	in [find cs ins cil]
    | RES (i1, i2, j) -> 
	(match (get i1 cs) with
	     None -> []
	   | Some c1 -> 
	       (match (get i2 cs) with
		    None -> []
		  | Some c2 -> 
		      let cil = exec_resolution cs c1 i1 c2 i2
		      in [find cs ins cil]))
    | ESP q ->
	[Esplitting.make_clause q];;


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
