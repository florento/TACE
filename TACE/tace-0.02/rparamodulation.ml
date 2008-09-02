
open List;;

open Trace;;
open Chrono;;
open Symbol;;
open Term;;
open Equation;;
open Trs;;
open Clausetype;;
open Clause;;


type monitor = {
  mutable rpa_time : chrono;
};;


let monitor =     
  {
    rpa_time = Chrono.make "ch_rparamodulation";
  };;


let rparamodulation_r1 (l,r) rc = 
  assert (not (Term.is_variable l));
  assert (Clause.get_type rc = REG);
  assert (Symbol.equal (Term.topsymbol l) (reg_get_function rc));
  (* inutile *)
  (* let c = Clause.map_var (Variable.set_color 1) rc in *)
  let pal = Clause.get_others POS rc in
  let h = assert (List.length pal = 1); hd pal in
  let q = Atom.get_predicate h in
  let tl = 
    assert (get_kind q = PREDICATE);
    assert (get_arity q = 1); 
    Atom.get_args h in
  let t = assert (List.length tl = 1); hd tl in
  (* matching, can not fail *)
  match (Matching.matcher t l) with
      None -> 
	error "Rparamodulation.rparamodulation_r1: matching failed\n";
	Clause.empty ()
    | Some s ->
	let sb = Substitution.map Term.block s in
	let nal = Clause.get_others NEG rc in
	let al  = List.map (Atom.substitute sb) nal in
	let a = Atom.make q [ (Term.block r) ] 
	in Clause.refresh (Clause.make [] al [] [] [a] []);;


let rec rparamodulation r c =
  let rec rparamodulation_r accu = function
      [] -> accu
    | e::l ->
	let f = reg_get_function c in
	let el = left e in
	  if (Symbol.is_blocked f)
	    || (Term.is_variable el)
	    || (not (Symbol.equal (Term.topsymbol el) f))
	  then 
	    rparamodulation_r accu l
	  else
	    let c1 = rparamodulation_r1 e c 
	    in rparamodulation_r (c1::accu) l in
    if is_regular c
    then begin
      Chrono.start monitor.rpa_time;
      let result = rparamodulation_r [] r in
	Chrono.stop monitor.rpa_time;
	result
    end      
    else begin 
      error "right-paramodulation implemented only for regular clauses\n"; 
      []
    end;;


