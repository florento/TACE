
open List;;
open Printf;;
open Format;;
open Stream;;

open Trace;;


let size_list f l = 
  List.fold_left (fun i a -> i + (f a)) 0 l;;


let meme eq a l = 
  List.exists (eq a) l;;

 
let rec clean eq = function
    [] -> []
  | a::l -> a::(clean eq (List.filter (fun b -> not (eq a b)) l));;


let is_clean eq l = 
  let rec is_clean1 = function
      [] -> 
	true
    | a::l1 ->
	(not (meme eq a l1)) && (is_clean1 l1)
  in is_clean1 l;;


let rec clean_ins eq a l = 
  if meme eq a l
  then l
  else a::l;;


let clean_union eq l1 l2 = 
  fold_left (fun l b -> clean_ins eq b l) l2 l1;;


let rec clean_rev_append eq l1 l2 = 
  let rec clean_rev_append1 l3 = function
      [] -> l3
    | a::l ->   
	if meme eq a l3
	then clean_rev_append1 l3 l
	else clean_rev_append1 (a::l3) l
  in clean_rev_append1 l1 l2;;

	
let list_equal eq l1 l2 = 
  (List.length l1 = List.length l2)
  && (List.for_all (fun a -> meme eq a l2) l1);;


let subseteq eq l1 l2 = 
  for_all (fun x -> exists (fun y -> eq x y) l2) l1;;


let rec mem_assoce eq x = function
    [] -> false
  | (a, _)::l when (eq a x) -> true
  | _::l -> mem_assoce eq x l;;


let rec assoce eq x = function
    [] -> raise Not_found
  | (a, b)::l when (eq a x) -> b 
  | _::l -> assoce eq x l;;


let find_pos p l = 
  let rec find_pos1 i = function
      [] -> None
    | x::l -> 
	if p x 
	then Some i 
	else find_pos1 (i+1) l
  in find_pos1 0 l;;


let find_allpos p l = 
  let rec find_allpos1 i accu = function
      [] -> List.rev accu
    | x::l -> 
	if p x 
	then find_allpos1 (i+1) (i::accu) l
	else find_allpos1 (i+1) accu l
  in find_allpos1 0 [] l;;


let rec iassoc x = function
    [] -> raise Not_found
  | (a, b)::l when b = x -> a
  | _::l -> iassoc x l;;


let rec iassq x = function
    [] -> raise Not_found
  | (a, b)::l when b == x -> a
  | _::l -> iassq x l;;


let rec imem_assoc x = function
    [] -> false
  | (a, b)::l when b = x -> true
  | _::l -> imem_assoc x l;;


let rec imem_assq x = function
    [] -> false
  | (a, b)::l when b == x -> true
  | _::l -> imem_assq x l;;


let rec multimap f = function
    [] -> []
  | a::l ->
      (map (fun x -> (x::l)) (f a))@(map (fun x -> a::x) (multimap f l));;


let rec multimap2 f = function
    [] -> []
  | a::l ->
      (map (fun (x, s) -> ((x::l), s)) (f a)) @ 
	(map (fun (x, s) -> ((a::x), s)) (multimap2 f l));;


let rec nthtl1 accu i = function
    [] -> begin
      error "Util.nthtl\n";
      invalid_arg "Util.nthtl";
    end
  | a::l ->
      if i = 0
      then (a, (rev_append accu l))
      else if i > 0
      then nthtl1 (a::accu) (i-1) l
      else invalid_arg "Util.nthtl";;


let nthtl l n = 
  nthtl1 [] n l;;


let replace l1 i a = 
  let rec replace1 i a = function
      [] ->  failwith "Util.replace: list too short"
    | b::l -> 
	if (i = 0)
	then a::l
	else b::(replace1 (i-1) a l) in
    replace1 i a l1;;


let replacel l1 i l2 =     
  let rec replace1 i lr = function
      [] -> lr
    | a::l -> 
	if i = 0 
	then lr@l
	else a::(replace1 (i-1) lr l)
  in
    if (i < 0)
    then invalid_arg (sprintf "Util.replace: position %i" i) 
    else replace1 i l2 l1;;


let remove l i =       
  let rec remove1 i = function
      [] -> failwith "Util.remove: list too short"
    | a::al -> 
	if i = 0 
	then al
	else a::(remove1 (i-1) al)
  in
    if (i < 0)
    then invalid_arg (sprintf "Util.remove: position %i" i) 
    else remove1 i l;;


let rec p_pplus n p =
  if n = 0
  then []
  else if (n > 0)
  then p::(p_pplus (n-1) (p+1))
  else invalid_arg (sprintf "Util.p_pplus %i" n);;
  

let list_meq eq l1 l2 = 
  let rec list_meq1 = function
      [], [], [] -> true
    | [], m::lm, [] -> failwith "list_meq"
    | [], lm, b::lr -> false
    | a::ll, lm, [] -> false
    | a::ll, lm, b::lr when (eq a b) -> list_meq1 (ll, [], (rev_append lm lr))
    | a::ll, lm, b::lr -> list_meq1 (a::ll, (b::lm), lr) in
    if ((List.length l1) = (List.length l2))
    then list_meq1 (l1, [], l2)
    else false;;
	

let max l = 
  let rec max1 m = function
      [] -> m
    | i::l -> 
	if i > m
	then max1 i l
	else max1 m l 
  in match l with
      [] -> invalid_arg "mac: empty list"
    | i::l -> max1 i l;;


let rec tail_cons c = function
    [] -> [c]
  | a::l -> a::(tail_cons c l);;


let rec deoptionize = function
    [] -> []
  | None::l -> deoptionize l
  | (Some e)::l -> e::(deoptionize l);;


let nthl l = 
  p_pplus (List.length l) 0;;


let mapi f = 
  let rec mapi1 i accu = function
      [] ->
	List.rev accu
    | a::l -> 
	let b = f a i 
	in mapi1 (i+1) (b::accu) l
  in mapi1 0 [];;


(***************)
(*   Streams   *)
(***************)


let is_empty st = 
  try 
    (Stream.empty st); true
  with 
      Stream.Failure -> false;;


let surepeek st = 
  match (Stream.peek st) with
      Some c -> c
    | None -> failwith "Parsing.surepeek";;


let peek_nth n st = 
  let l = npeek n st in 
    try 
      List.nth l n     
    with
	_ -> failwith "Parsing.peek_nth";;


(***************)
(*    Bool     *)
(***************)

let xor a b = 
  (a || b) && (not (a && b));;

(***************)
(*   Output    *)
(***************)

let rec list_to_string f = function
    [] -> ""
  | x::[] -> (f x)
  | x::l -> 
      let s = (f x) in
	if ((String.length s) = 0)
	then (list_to_string f l)
	else sprintf "%s, %s" s (list_to_string f l);;


let dump_list printer al = 
  let rec dump_list1 pr = function
      [] -> ()
    | [x] -> (pr x)
    | x::l -> 
	(pr x);
	print_string ",";
	print_space ();
	(dump_list1 pr l) in
    open_box 0;
    (dump_list1 printer al);
    close_box ();;
      

