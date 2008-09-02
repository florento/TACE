
open List;;

open Trace;;
open Chrono;;
open Util;;
open Myqueue;;
open Clause;;
open Clausetype;;
open Clauseset;;
open Subsumption;;
open Instruction;;
open Interpreter;;


type queuemode = 
    QUEUE_INSTR 
  | QUEUE_CLAUSE;;


let queuemode_to_string = function
    QUEUE_INSTR -> "QUEUE_INSTR"
  | QUEUE_CLAUSE -> "QUEUE_CLAUSE";;
    

type haltmode = 
    HALT_FIRST 
  | HALT_CONTINUE;;


let haltmode_to_string = function
    HALT_FIRST -> "HALT_FIRST"
  | HALT_CONTINUE  -> "HALT_CONTINUE";;
    

type clausequeue =  
    { 
      mutable q1 : (instruction * int) queue;
      mutable q2 : (clause * instruction) queue;
    }      



type monitor = {
  mutable sat_loop      : int;
  mutable sat_init      : int;
  mutable sat_clauses   : int;
  mutable sat_added     : int;
  mutable sat_deleted   : int;
  mutable sat_max_queue : int;
  mutable sat_time      : chrono;
};;


let monitor =     
  {
    sat_loop  = 0;
    sat_init  = 0;
    sat_clauses = 0;
    sat_added = 0;
    sat_deleted = 0;
    sat_max_queue = 0;    
    sat_time = Chrono.make "ch_saturation";
  };;



(*******************************)
(* Queues                      *)
(*******************************)


let ord1 (_, s1) (_, s2) = 
  s1 > s2;;


let eq1 (i1, s1) (i2, s2) = 
  (Instruction.equal i1 i2);;
  (* && (s1 = s2);; *)


let eq2 (c1, i1) (c2, i2) = 
  (Instruction.equal i1 i2);;


let ord2 (c1, _) (c2, _) = 
  (Clause.size c1) > (Clause.size c2);;


let init_queue cs cl qmode = 
  assert (debug_b "Saturation.init_queue %s\n" (queuemode_to_string qmode));
  let cq = 
    { q1 = Myqueue.make (); q2 = Myqueue.make () } in 
  let cil = List.flatten (List.map (fun c -> exec_new cs c) cl)
  in (match qmode with
	  QUEUE_INSTR ->
	    List.iter 
	      (fun (c, ins) -> Myqueue.add (ins, Clause.size c) cq.q1 ord1 eq1) 
	      cil
       | QUEUE_CLAUSE ->
	   (* TBC: redondance... *)
	   List.iter 
	     (fun (c, ins) -> Myqueue.add (c, ins) cq.q2 ord2 eq2) 
	     cil
     );
    cq;;


let is_empty cq qmode =
  match qmode with
      QUEUE_INSTR ->
	Myqueue.is_empty cq.q1
    | QUEUE_CLAUSE ->
	Myqueue.is_empty cq.q2;;


let queue_length cq = function
    QUEUE_INSTR ->
      Myqueue.length cq.q1
  | QUEUE_CLAUSE ->
      Myqueue.length cq.q2;;


(** [next cq]
    @return the next clause from [cq], queue of type [qmode],
    with the instruction which produced this clause. *)
let rec next cq cs qmode = 
  trace 5 "Saturation.next <%s>, queue length before pop = %i\n" 
    (queuemode_to_string qmode)
    (queue_length cq qmode);
  match qmode with
      QUEUE_INSTR ->
	let (ins, _) = Myqueue.next cq.q1 in 
	let cl = Interpreter.exec cs ins in
	  if (List.length cl = 0)
	  then next cq cs qmode
	  else begin
	    assert (List.length cl = 1); 
	    ((hd cl), ins)
	  end
    | QUEUE_CLAUSE ->
	Myqueue.next cq.q2;;


let add_queue c ins cq qmode = 
  assert (debug_b "Saturation.add_queue %s:%s <%s>\n" 
	    (Clause.to_stringt c)
	    (Instruction.to_string ins)
	    (queuemode_to_string qmode));
  match qmode with
      QUEUE_INSTR ->
	Myqueue.add (ins, (Clause.size c)) cq.q1 ord1 eq1
    | QUEUE_CLAUSE ->
	Myqueue.add (c, ins) cq.q2 ord2 eq2;;


let trace_add c i ins cs = 
  match ins with
      NIL -> ()
    | NEW _ ->
	trace 3 "%i: %s < initial clause >\n" i (Clause.to_stringt c)
    | LPA (i1, _) ->
	trace 3 "%i: %s < narrowing of one equation in clause %i: %s >\n"
	  i (Clause.to_stringt c) 
	  i1 (Clause.to_stringt (get_clause i1 cs))
    | SOL (i1, _) ->
	trace 3 "%i: %s < solving equations in clause %i: %s >\n"
	  i (Clause.to_stringt c) 
	  i1 (Clause.to_stringt (get_clause i1 cs)) 
    | RPA (i1, _) ->
	trace 3 "%i: %s < right-paramodulation into clause  %i: %s >\n"
	  i (Clause.to_stringt c)
	  i1 (Clause.to_stringt (get_clause i1 cs)) 
    | RES (i1, i2, _) ->
	trace 3 "%i: %s < resolution of clause %i: %s into clause %i: %s >\n"
	  i (Clause.to_stringt c)
	  i1 (Clause.to_stringt (get_clause i1 cs)) 
	  i2 (Clause.to_stringt (get_clause i2 cs))
    | ESP q ->
	trace 3 "%i: %s < epsilon-splitting >\n"
	  i (Clause.to_stringt c);;
	

(** application of non-eager inferences followed by eager inferences
    between a new clause added and the clauses previously added.

    [interaction c i cs]
    @return the list of pairs made of a clause 
    and the instruction who returned this clause 
    resulting of interaction between the clause [c],
    at position [i] in [cs], with the other clauses of [cs]. *)
let interaction c i cs =
  assert (debug_b "Saturation.interaction %i: %s\n" i 
	    (Clause.to_stringt c));
  match (get_type c) with
      UNDEF -> 
	warning "Saturation.interaction: unknown type\n"; []
    | EQ ->
	exec_solve cs c i
    | REG -> 
	List.rev_append
	  (exec_rparamodulation cs c i)
	  (exec_resolutions cs c i)
    | SPOS 
    | SPLIT 
    | DEEP 
    | ONEVAR 
    | UGOAL 
    | UNSEL -> 
	exec_resolutions cs c i;;
    

let add_cs c ins cs qmode = 
  assert (debug_b "Saturation.add_cs %s  (%s)\n" 
	    (Clause.to_stringt c)
	    (Instruction.to_string ins));
  if (forward_subsume cs c)
  then begin
    trace 4 "Saturation: clause %s subsumed, not added\n" 
      (Clause.to_string c);
    []
  end
  else begin
    monitor.sat_added <- monitor.sat_added + 1;
    (* removes clauses of cs subsumed by c *)
    backward_subsume c cs;
    monitor.sat_deleted <- Clauseset.deleted cs;
    (* add c to cs *)
    let i = Clauseset.add c ins cs in
      (if get_trace_level () >= 3 then trace_add c i ins cs);
      (* apply non-eager inferences with clauses of cs and trs *)	
      interaction c i cs 
  end;;


(*   List.filter (fun (c, ins) -> not (Clause.is_empty c)) cil;; *)
let remove_empty empty_pos cs cil = 
  let rec remove_empty1 aux = function
      [] -> List.rev aux
    | (c, ins)::l -> 
	if (Clause.is_empty c)
	then begin
	  (* the empty clause is added to the clauseset to be able
	     to reconstruct the history.
	     It won't arm for further interactions. *)
	  empty_pos := (Clauseset.add c ins cs)::!empty_pos;
	  (* dont forget to remove the other occurences of empty *)
	  remove_empty1 aux l
	end
	else 
	  remove_empty1 ((c, ins)::aux) l 
  in remove_empty1 [] cil;;


let saturate cs cl queue_mode halt_mode = 
  assert (debug_b 
	    "Saturation.saturate %s %s, \
             %i saturated clauses, %i rewrite rules, %i new clauses\n"
	    (queuemode_to_string queue_mode)
	    (haltmode_to_string halt_mode)
	    (Clauseset.length cs) 
	    (Trs.length (get_trs cs))
	    (List.length cl));
  monitor.sat_init <- (List.length cl);
  let cq = init_queue cs cl queue_mode in
  let empty_pos = ref [] in
    trace 2 "Saturation.saturate: START\n";
    Chrono.start monitor.sat_time;
    while not ( (is_empty cq queue_mode) 
	       || ((halt_mode = HALT_FIRST) && !empty_pos <> []))
    do     
      assert (debug_b 
		"Saturation.saturate LOOP: saturated clauses %i, queue %i\n"
	(Clauseset.length cs)
	(queue_length cq queue_mode));
      monitor.sat_loop <- monitor.sat_loop + 1;
      (* NEXT *)
      let (c, ins) = next cq cs queue_mode in	
	if (Clause.get_type c = UNDEF)
	then begin
	  error "Saturation.saturate: UNDEF clause %s < %s >\n"
	    (Clause.to_string c) (Instruction.to_string ins);
	  failwith "Saturation.saturate";
	end
	else 
	  (* add cs and compute interaction with cs *)
	  let cil1 = 
	    assert (debug_b "Saturation.saturate NEXT = %s (%s)\n" 
		      (Clause.to_stringt c)
		      (Instruction.to_string ins));
	    (* trace 5 "Saturation NEXT %s <%s>\n"  
	       (Clause.to_stringt c) (Instruction.to_string ins); *)
	    (add_cs c ins cs queue_mode) in
	    (* test if empty clause is produced *)
	  let cil2 =  
	    remove_empty empty_pos cs cil1 in
	    
	    (* push non-empty clauses to the queue *)
	    List.iter (fun (c2, ins2) -> 
			 assert (debug_b "Saturation.saturate ADD QUEUE %s (%s)\n"
				   (Clause.to_stringt c2)
				   (Instruction.to_string ins2));
			 (* ADD QUEUE *)
			 add_queue c2 ins2 cq queue_mode) cil2;
	    (* update number of clauses *)
	    monitor.sat_clauses <- (Clauseset.length cs);
	    (* update max queue length *)
	    let qlen = (queue_length cq queue_mode) in
	      if qlen > monitor.sat_max_queue 
	      then monitor.sat_max_queue <- qlen;
    done;
    Chrono.stop monitor.sat_time;
    assert (debug_b "Saturation.saturate FINISH\n");
    assert (debug_b "Saturation.saturate: clause queue is %s\n" 
	      (if (is_empty cq queue_mode)
	       then "empty"
	       else "not empty"));
    List.map (fun p -> History.of_clauseset cs p) !empty_pos;; 


