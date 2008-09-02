(** Various function for parsing *)

open List;;
open Printf;;
open Stream;;
open Trace;;
open Location;;
open Util;;


exception Parse_error of string * location;;


(*******************)
(*    Utilities    *)
(*******************)

(* globale: current stream *)
(* let st = ref (Stream.of_list []);; *)

(* globale *)
let current_loc = 
  { 
    Location.file = "";
    Location.start_line = 1;
    Location.end_line = 1;
    Location.start_char = 0;
    Location.end_char = 0 
  };;


let get_location () = 
  current_loc;;


let reset_location () = 
  current_loc.file <- "";
  current_loc.start_line <- 1;
  current_loc.end_line <- 1;
  current_loc.start_char <- 0;
  current_loc.end_char <- 0;;


let restart_location () = 
  current_loc.start_line <- current_loc.end_line;
  current_loc.start_char <- current_loc.end_char;;


let set_filename name = 
  current_loc.file <- name;;


let get_filename () = 
  current_loc.file;;


let loc_newline () = 
  current_loc.end_line <- current_loc.end_line+1;
  current_loc.end_char <- 0;;


let loc_incrc () = 
  current_loc.end_char <- current_loc.end_char+1;;


let parse_error msg = 
  Parse_error (msg, current_loc);;


let is_digit = function
    '0'..'9' -> true
  | _ -> false;;


let peek st = 
  match (Stream.peek st) with
      None -> None
    | Some c -> 
	c;;


let surepeek st = 
  match (peek st) with
      None -> failwith "surepeek: empty stream"
    | Some c -> c;;


let nextchar st = 
  if (Util.is_empty st)
  then raise Stream.Failure 
  else 
    let c = Stream.next st in      
      (* extend the current location with the character read *)
      (if c = '\n' 
       then 
	 loc_newline ()
       else 
	 loc_incrc ());
      c;;
	

let skip st = 
  (* extend the current location with the character skiped *)
  (let p = Stream.peek st in match p with
      None -> ()
    | Some '\n' ->
	loc_newline ()
    | Some _ -> 
	loc_incrc ());
  Stream.junk st;;
 

let scan c st = 
  try 
    (* extend the current location with the character scaned *)
    if c = '\n'
    then loc_newline ()
    else loc_incrc ();
    let c1 = Stream.next st in
      if c1 = c 
      then ()
      else begin
	error "Parsing.scan: %c expected, %c read\n" c c1;
	raise (parse_error (sprintf "'%c' expected" c))
      end
  with 
      Stream.Failure -> 
	error "Parsing.scan: Stream fialure, %c expected\n" c;
	raise (parse_error (sprintf "'%c' expected" c));;
      
  

(*******************)
(*     Parsers     *)
(*******************)

(* moved to data modules, see parsing.readme *)

(*******************)
(*     Output      *)
(*******************)

