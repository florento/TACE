
open List;;
open Printf;;

open Trace;;
open Chrono;;
open Util;;
open Variable;;
open Symbol;;
open Term;;
open Atom;;
open Unification;;
open Clause;;
open Clausetype;;
open Substitution;;


type monitor = {
  mutable res_tried : int;
  mutable res_success : int;
  mutable res_fail_table : int;
  mutable res_fail_other : int;
  mutable res_time : chrono;
};;


let monitor =     
  {
    res_tried = 0;
    res_fail_table = 0;
    res_fail_other = 0;
    res_success = 0;
    res_time = Chrono.make "ch_resolution";
  };;
  

(* TBD: priority to test predicates (ordered strategy) *)


let resolution c1 c2 = 
  assert (debug_b "Resolution.resolution: %s on %s\n"
	    (Clause.to_stringt c1)
	    (Clause.to_stringt c2));
  monitor.res_tried <- monitor.res_tried + 1;
  Chrono.start monitor.res_time;
  if resolution_table (Clause.get_type c1) (Clause.get_type c2)
  then begin
    assert (not (is_goal c1));
    assert ((get_equations POS c1) = []);
    (* variables of [c2] are uncolored *)
    assert (Clause.for_all_var (fun v -> Variable.get_color v = 0) c2);
    let c11 = Clause.map_var (Variable.set_color 1) c1 in
    let h11 = hd (get_atoms POS c11) in
    let sal = select_atoms_unif h11 c2 in 
    let cl = 
      assert (debug_b "Resolution.resolution: %i selected\n"
		(List.length sal));     
      List.map
	 (fun (s, c20) -> 
	    let c21 = 
	      if Substitution.for_all_domain 
		(fun x -> ((Variable.get_color x) = 1)) s
		(* no variable of [c2] in the domain of subst [s] *)
	      then c20
		(* otherwise apply unifier *)
	      else (Clause.substitute s c20) 
	    in (Clause.refresh
		  (Clause.merge (Clause.substitute s c11) c21)))
	 sal 
    in 
      Chrono.stop monitor.res_time; 
      if (cl = []) 
      then monitor.res_fail_other <- monitor.res_fail_other + 1
      else monitor.res_success <- monitor.res_success + 1;    
      cl      
  end
  else begin
    assert (debug_b "Resolution.resolution: not in table\n");
    Chrono.stop monitor.res_time;
    monitor.res_fail_table <- monitor.res_fail_table + 1;
    []
  end;;



