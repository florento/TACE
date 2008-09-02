
open List;;

open Trace;;
open Chrono;;
open Symbol;;
open Term;;
open Equation;;
open Unification;;
open Trs;;
open Clausetype;;
open Clause;;
open Lparamodulation;;


type monitor = {
  mutable sol_time : chrono;
}


let monitor =     
  {
    sol_time = Chrono.make "ch_solving";
  };;


let solves ecl = 
  let rec solves1 accu = function
      [] -> rev accu
    | (e, c)::l ->
	(match (unify (left e) (right e)) with
	     None -> solves1 accu l
	   | Some s ->
	       let c1 = Clause.refresh (Clause.substitute s c) 
	       in solves1 (c1::accu) l)
  in solves1 [] ecl;;


let solve c = 
  assert (debug_b "Lparamodulation.solve %s\n" (Clause.to_string c));  
  solves (select_equations c);;


let solve_all r c = 
  assert (debug_b "Lparamodulation.solve_all %s\n" (Clause.to_string c));
  let rec solve_all2 solved tosolve = 
    (* TBR *)
    debug "Solving.solve_all2: \n";
    debug "Solving.solve_all2: SOLVED\n";
    List.iter 
      (fun x -> debug   "Solving.solve_all2: %s\n" (Clause.to_stringt x)) 
      solved;
    assert (debug_b "Solving.solve_all2: TO SOLVE\n");
    List.iter 
      (fun x -> debug   "Solving.solve_all2: %s\n" (Clause.to_stringt x))
      tosolve;
    (* end TBR *)
    match tosolve with
	[] -> solved
    | c1::l ->
	if (get_type c1) = EQ
	then 
	  solve_all2
	    solved 
	    (rev_append
	       (rev_append (solve c1) (lparamodulation r c1)) l) 
	else 
	  solve_all2 (c1::solved) l 
  in 
    Chrono.start monitor.sol_time;  
    let result = 
      solve_all2 [] [c] in
      Chrono.stop monitor.sol_time;  
      result;;


