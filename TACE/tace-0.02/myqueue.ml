(** STUB. implement it with heap (prority queue) *)

open List;;
open Format;;

open Trace;;
open Util;;

type 'a queue = 
    { mutable p : 'a list }


exception Empty


let make () = 
  { p = [] };;


let is_empty q = 
  q.p = [];;


let add c q ord eq = 
  let rec add1 c = function
      [] -> [ c ]
    | c1::l when (ord c c1) -> 
	c1::(add1 c l)
    | il ->  
	c::il
  in 
    if meme eq c q.p
    then ()
    else q.p <- add1 c q.p;;


let length q =
  List.length q.p;;


let next q =
  if is_empty q
  then 
    raise Empty
  else 
    let n = hd q.p in
      q.p <- tl q.p;
      n;;


let dump q dumpe = 
  let n = (length q) in
    open_box 0; 
    debug "Dump clausequeue: ";
    print_int n;
    print_string " clauses";
    print_newline ();
    for i = 0 to (n-1) do
      let c = List.nth q.p i in
	open_box 0; 
	debug "%4i " i;
	print_space ();
	dumpe c;
	print_newline ();
    done;;
