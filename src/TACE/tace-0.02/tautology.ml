
open List;;

open Trace;;
open Chrono;;
open Util;;
open Atom;;
open Equation;;
open Clause;;
open Clausetype;;



type monitor = {
  mutable tau_tested : int;
  mutable tau_success : int;
  mutable tau_time : chrono;
}


(** global monitor, uptated by the module *)
let monitor =
  {
    tau_tested  = 0;
    tau_success = 0;
    tau_time    = Chrono.make "ch_tautology";
  };;


(** [meme3 eq l1 l2]
    @return 0 if l1 = []
            1 if l1 = [a] and a is in l2
            2 if l1 = [a] and a is not in l2 

    The length of [l1] must be at most 1. *)
let meme3 eq l1 l2 = 
  if l1 = []
  then 0
  else begin
    assert (List.length l1 = 1);
    if Util.meme eq (hd l1) l2
    then 1
    else 2
  end;;
  

(* test whether the (only) positive literal is one of the negative literals *)
let tautology2 c = 
  if is_goal c
  then false
  else let psl = get_splittings POS c in
    if (List.length psl > 0)
    then begin
      assert (List.length psl = 1);
      Util.meme Atom.equal (hd psl) (get_splittings NEG c)
    end
    else let asl = get_others POS c in
      if (List.length asl > 0)
      then begin
	assert (List.length asl = 1);
	Util.meme Atom.equal (hd asl) (get_others NEG c)
      end
      else let esl = get_equations POS c in
        if (List.length esl > 0)
	then begin
	  assert (List.length esl = 1);
	  Util.meme Equation.equal (hd esl) (get_equations NEG c)
	end
	else false;;


let tautology1 c = 
  assert (is_horn c);
  match (get_type c) with
      UNDEF -> 
	tautology2 c
    | SPOS -> 
	false
    | REG -> 
	false
    | EQ
    | SPLIT
    | DEEP ->
	tautology2 c
    | ONEVAR ->
	assert (List.length (get_others POS c) = 1);
	Util.meme Atom.equal (hd (get_others POS c)) (get_others NEG c)   
    | UGOAL -> 
	false
    | UNSEL -> 
	false;;


let tautology c = 
  debug "Tautology.tautology %s\n" (Clause.to_string c);
  monitor.tau_tested <- monitor.tau_tested + 1;
  Chrono.start monitor.tau_time;
  if (tautology1 c)
  then begin
    trace 4 "%s is a tautology\n" (Clause.to_string c);
    Chrono.stop monitor.tau_time;
    monitor.tau_success <- monitor.tau_success + 1;
    true;
  end
  else begin
    Chrono.stop monitor.tau_time;
    false
  end;;

