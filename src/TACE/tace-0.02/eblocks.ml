
open List;;
open Printf;;
open Format;;

open Trace;;
open Util;;
open Symbol;;
open Variable;;
open Renaming;;
open Term;;
open Atom;;


type eblocks = 
    (variable * (symbol list)) list;;


let make () = 
  assert (debug_b "Eblocks.make\n");
  ([] : eblocks);;


let to_string ebl =
  let eblock_to_string (x, ql) = 
    let sx = Variable.to_string x
    in list_to_string (fun q -> sprintf "%s(%s)" (Symbol.to_string q) sx) ql
  in list_to_string 
       (fun eb -> "["^(eblock_to_string eb)^"]") ebl;;
(*   Util.list_to_string Atom.to_string (to_atoms ebl);; *)

(*
let to_string ebl = 
  let eblock_to_string x ql = 
    "("^(Variable.to_string x)^", "^(Util.list_to_string Symbol.to_string ql) 
  in Util.list_to_string eblock_to_string ebl;;
*)


let to_atom1 (x, ql) = 
  let t = Term.of_variable x 
  in List.map (fun p -> Atom.make p [t]) ql;;


let rec to_atoms = 
  List.map to_atom1;;


let mem x eb =
  mem_assoce Variable.equal x eb;;


let get x ebl = 
  assert (mem x ebl);
  assoce Variable.equal x ebl;;


let get_predicates ebl = 
  List.map snd ebl;;


let mem_pred q ebl = 
  List.exists  (fun (_, ql) -> meme Symbol.equal q ql) ebl;; 


let is_empty ebl = 
  ebl = [];;


let add x q eb = 
  assert (debug_b "Eblocks.add %s %s\n" 
	    (Variable.to_string x) (Symbol.to_string q));
  assert (get_kind q = PREDICATE);
  assert (get_arity q = 1);
  let rec add1 aux x q = function
      [] -> rev_append aux [ (x, [q]) ]
    | (y, ql)::l when (Variable.equal x y) ->
	(y, (clean_ins Symbol.equal q ql))::l
    | (y, ql)::l ->
	add1 ((y, ql)::aux) x q l 
  in add1 [] x q eb;;


let add_atom a eb =
  assert (debug_b "Eblocks.add_atom %s\n" (Atom.to_string a));
  assert (Atom.is_epsilon a);
  assert (List.length (Atom.get_args a) = 1);
  add (Term.to_variable (hd (Atom.get_args a))) (Atom.get_predicate a) eb;;


let of_atoms al = 
  let rec adda eb = function
      [] -> eb
    | a::l -> 
	let q = Atom.get_predicate a in
	let tl = 
	  assert (get_kind q = PREDICATE);
	  assert (get_arity q = 1);
	  Atom.get_args a in 
	let t = 
	  assert (List.length tl = 1);
	  hd tl in
	let x = 
	  assert (Term.is_variable t);
	  Term.to_variable t
	in adda (add x q eb) l 
  in adda (make ()) al;;


let add_block (x, ql) ebl = 
  let rec add_block1 accu = function
      [] -> (x, ql)::accu
    | (y, pl)::l -> 
	assert (not (exists (fun (y1, _) -> Variable.equal y1 y) l));
	if (Variable.equal y x)
	then rev_append accu ((x, (clean_union Symbol.equal ql pl))::l)
	else add_block1 ((y, pl)::accu) l
  in add_block1 [] ebl;;
  

(* tail recursive, reverse list *)
let rec append ebl1 ebl2 = 
  assert (debug_b "Eblocks.append\n");
  fold_left (fun ebl eb -> add_block eb ebl) ebl2 ebl1;;
    

let vars ebl = 
  List.map fst ebl;;
  

let equal (x1, pl1) (x2, pl2) = 
  assert (is_clean Symbol.equal pl1);
  assert (is_clean Symbol.equal pl2);  
  (Variable.equal x1 x2)
  && (list_equal Symbol.equal pl1 pl2);;


let rec rename ren = function
    [] -> []
  | (x, ql)::l -> (Renaming.apply ren x, ql)::(rename ren l);;


let refresh ebl = 
  assert (debug_b "Eblock.refresh\n");
  let ren = Renaming.make () 
  in rename ren ebl;;


let rec map_var f = function
    [] -> []
  | (x, ql)::ebl -> (f x, ql)::(map_var f ebl);;


let length ebl = 
  size_list (fun (_, ql) -> (List.length ql)) ebl;;
  

let size ebl = 
  2 * (length ebl);;


let is_linear ebl = 
  for_all (fun (_, ql) -> List.length ql = 1) ebl;;
  

let dump1 (x, pl) =
  open_box 0; 
  print_string (Variable.to_string x);
  print_string ":";
  print_space ();
  Util.dump_list (fun p -> print_string (Symbol.to_string p)) pl;
  print_space ();
  close_box ();;
	

let dump ebl = 
  dump_list dump1 ebl;;


let check ebl = 
  let rec check1 = function
      [] -> true
    | q::l -> 
	Trace.check 
	  ((get_kind q = PREDICATE) 
	   && (get_arity q = 1)
	   && (not (meme Symbol.equal q l))
	   && (check1 l))
	  "Eblocks.check: symbol %s not predicate or not unary or not clean\n"
	  (Symbol.to_string q)
  in 
    (Trace.check 
       (Util.is_clean (fun (x1, _) (x2, _) -> (Variable.equal x1 x2)) ebl)
       "Eblocks.check: variable repetition\n")
    && List.for_all (fun (_, ql) -> check1 ql) ebl;;

