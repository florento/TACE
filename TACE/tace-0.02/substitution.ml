
open List;;
open Printf;;

open Trace;;
open Util;;
open Symbol;;
open Variable;;
open Term;;
open Equation;;
open Atom;;
open Clause;;


type substitution = 
    (variable * term) list;;


let make () = 
  [];;

let to_string subst = 
  let rec binding_to_string (x, t) = 
    sprintf "%s = %s" (Variable.to_string x) (Term.to_string t) in 
  let rec bindinglist_to_string = function 
      [] -> ""
    | [b] -> (binding_to_string b)
    | b::l -> (binding_to_string b)^"; "^(bindinglist_to_string l) 
  in (bindinglist_to_string subst);;


let to_term subst = 
  let binding_to_term (x, t) = 
    T (bind_symbol, [V x; t]) in
  let rec to_term1 = function  
    [] -> T (empty_symbol, [])
    | b::l -> 
	T (cons_symbol, [(binding_to_term b); (to_term1 l)]) 
  in to_term1 subst;;


let of_term t =
  let rec binding_of_term = function
      V _ -> failwith "Substitution.of_term"
    | T (f, [V x; t]) when Symbol.equal f bind_symbol -> (x, t)
    | T _ -> failwith "Substitution.of_term"
  and of_term1 = function
    V _ -> failwith "Substitution.of_term"
  | T (f, tl) when Symbol.equal f empty_symbol -> []
  | T (f, [b; t]) when Symbol.equal f cons_symbol -> 
      (binding_of_term b)::(of_term1 t)
  | T _ -> failwith "Substitution.of_term"
  in of_term1 t;;


let mem x subst = 
  Util.mem_assoce Variable.equal x subst;;


let assoc x subst = 
  Util.assoce Variable.equal x subst;;


let apply s x = 
  if mem x s
  then assoc x s
  else Term.of_variable x;;


let add x t subst = 
  assert (debug_b "Substitution.add %s %s\n" 
	    (Variable.to_string x) (Term.to_string t));
  assert (not (mem x subst));
  (x, t)::subst;;
(*    warning "Substitution.add: var %s already bound, not added\n"  
      (Variable.to_string x); *)


let domain  = 
  List.map fst;;


let image = 
  List.map snd;;


let length = 
  List.length;;


let size = 
  fold_left (fun n (_, t) -> n + (Term.size t)) 0;;


let is_triangle subst = 
  let rec is_triangle1 = function
      [] -> 
	true
    | x::l -> 
	let t = (assoc x subst) in
	let vars = (Term.vars t) 
	in (List.for_all 
	      (fun y -> not (Util.meme Variable.equal y l)) 
	      (Term.vars (assoc x subst)))
	   && (is_triangle1 l)
  in is_triangle1 (domain subst);;
  

let rec substitute subst = function
    V i as t -> 
      if mem i subst
      then assoc i subst
      else t
  | T (sy, tl) -> 
      T (sy, List.map (substitute subst) tl);;




let map f = 
  List.map (fun (x, t) -> (x, f t));;


let rec for_all_domain pred =
  List.for_all (fun (x, _) -> (pred x))
    
  
let rec for_all_image pred = 
  List.for_all (fun (_, t) -> (pred t));;


let rec memcheck = function
    [] -> true
  | (a, b)::l -> 
      if (mem a l)
      then 
	if Term.equal b (assoc a l)
	then memcheck l
	else false
      else (memcheck l);;


let rec clean = function
    [] -> []
  | (x, V y)::sl when Variable.equal x y -> (clean sl)
  | ((x, t) as b)::l -> 
      if (mem x l)
      then 
	(warning "Substitution.clean: multiple bindings for var %s,\
          keeping the last one\n"
	  (Variable.to_string x);      
	 clean l)
      else 
	b::(clean l);;


let minus subst1 subst2 =
  let rec minus1 subst = function
      [] -> []
    | (x, t)::sl -> 
	if (mem x subst)
	then minus1 subst sl
	else (x,t)::(minus1 subst sl)
  in 
    assert (debug_b "minus %s and %s returns %s\n"
      (to_string subst1)
      (to_string subst2)
      (to_string (minus1 subst2 subst1)));  
    minus1 subst2 subst1;;


let compose subst1 subst2 = 
  let subst11 = map (substitute subst2) subst1 in
  let subst21 = (minus subst2 subst1) in
  let subst12 = (clean subst11) in 
    assert (debug_b "compose %s and %s returns %s\n" 
	      (to_string subst1)
	      (to_string subst2)
	      (to_string (subst12@subst21)));
    subst12@subst21;;


let rec disjoint_compose subst1 subst2 = 
  let subst11 = map (substitute subst2) subst1 
  in subst11@subst2;;
  
  
let rec uniono s1 = function
    [] -> Some s1
  | (x, t)::s2 -> 
      if mem x s1
      then 
	if Term.equal (assoc x s1) t
	then (uniono s1 s2)
	else None
      else uniono ((x, t)::s1) s2;;


let compatible s1 s2 = 
  match (uniono s1 s2) with
      None -> false
    | Some _ -> true;;


(*
let union s1 s2 = 
  match (uniono s1 s2) with
      Some s -> s
    | None -> invalid_arg "Substitution.union: incompatible";;
*)

(* fast version, does not check for compatibility *)
let rec union s1 s2 = 
  let rec union1 s aux = function
      [] -> rev_append aux s
    | (x, t)::l -> 
	if mem x s
	then union1 s aux l
	else union1 s ((x, t)::aux) l
  in union1 s2 [] s1;;


let rec find_double = function
    [] -> None
  | (x, t)::l -> 
      if (mem x l)
      then Some x
      else find_double l;;


let has_double s = 
  (find_double s) <> None;;


let block = 
  map Term.block;;


let unblock = 
  map Term.unblock;;


let map_var f = 
  List.map (fun(x, t) -> (f x, Term.map_var f t));;


let equal s1 s2 = 
  assert (debug_b "Substitution.equal\n");
  assert (Util.is_clean (fun (x1, _) (x2, _) -> Variable.equal x1 x2) s1);
  assert (Util.is_clean (fun (x1, _) (x2, _) -> Variable.equal x1 x2) s1);
  Util.list_equal 
    (fun (x1, t1) (x2, t2) -> (Variable.equal x1 x2) && (Term.equal t1 t2))
    s1 s2;;


let check_function s = 
  (find_double s) = None;;


let check s = 
  (Trace.check 
     (List.for_all (fun (x, t) -> (Variable.check x) && (Term.check t)) s)
     "Substitution.check: problem with Variable.check or Term.check\n")
  &&
    (Trace.check 
       (has_double s)
       "Substitution.check: variable %s defined twice\n"
       (Variable.to_string 
	  (match (find_double s) with Some x -> x | None -> failwith "oh")));;

