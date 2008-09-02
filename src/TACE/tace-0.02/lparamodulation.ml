
open List;;

open Trace;;
open Chrono;;
open Symbol;;
open Term;;
open Equation;;
open Trs;;
open Rewriting;;
open Narrowing;;
open Clausetype;;
open Clause;;


type monitor = {
  mutable lpa_time : chrono;
}


let monitor =     
  {
    lpa_time = Chrono.make "ch_lparamodulation";
  };;


let lparamodulations r ecl = 
  let rec lparamodulations1 r accu = function
      [] -> rev accu
    | (e, c)::l ->
	let esl = Narrowing.narrow_equation r e in
	let cl = List.map 
	  (fun (e1, s) -> 
	     let e2 = equation_normalize r (Equation.substitute s e1) in
	     let c1 = Clause.substitute s c in
	     let c2 = Clause.make [] [] [e2] [] [] []
	     in Clause.refresh (Clause.merge c2 c1)) esl	  
	in lparamodulations1 r (rev_append cl accu) l
  in lparamodulations1 r [] ecl;;


let lparamodulation r c =
  assert (debug_b "Lparamodulation.lparamodulation %s\n" (Clause.to_string c));
  lparamodulations r (select_equations c);;



