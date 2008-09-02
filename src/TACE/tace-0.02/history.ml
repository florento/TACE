
open Format;;

open Trace;;
open Symbol;;
open Clause;;
open Instruction;;
open Clauseset;;


type history = 
    H_NIL of int
  | H_LPA of int * history
  | H_SOL of int * history
  | H_RPA of int * history
  | H_NEW of int
  | H_RES of int * history * history
  | H_ESP of int * symbol;;


let rec of_clauseset cs i = 
  let ins = Clauseset.get_instruction i cs
  in match ins with 
      NIL -> 
	H_NIL i
    | LPA (i1, _) ->
	H_LPA (i, (of_clauseset cs i1))
    | SOL (i1, _) ->
	H_SOL (i, (of_clauseset cs i1))
    | RPA (i1, _) ->
	H_RPA (i, (of_clauseset cs i1))
    | NEW _ -> 
	H_NEW i
    | RES (i1, i2, _) -> 
	H_RES (i, (of_clauseset cs i1), (of_clauseset cs i2))
    | ESP q ->
	H_ESP (i, q);;


let get_index = function
    H_NIL i -> i
  | H_LPA (i, _) -> i
  | H_SOL (i, _) -> i
  | H_RPA (i, _) -> i
  | H_NEW i -> i
  | H_RES (i, _, _) -> i
  | H_ESP (i, _) -> i;;



(**********************************************)
(**            Output and debug               *)
(**********************************************)


let dump_clause i cs = 
  let c = Interpreter.recover i cs in
    open_box 0; 
    print_string "clause ";
    print_int i;
    print_char ':';
    print_space ();
    print_string (Clause.to_stringt c);
    close_box ();;


let rec dump cs = function
    H_NIL i ->
      open_box 2;
      dump_clause i cs;
      print_space ();
      print_string "by";
      print_space ();
      print_string "NIL";
      close_box ()      
  | H_LPA (i, h) ->
      open_box 2;
      dump_clause i cs;
      print_space ();
      print_string "by";
      print_space ();
      print_string "LPA";
      print_space ();
      print_int (get_index h);
      print_space ();
      print_newline ();
      dump cs h;
      print_newline ();
      close_box ()            
  | H_SOL (i, h) ->
      open_box 2;
      dump_clause i cs;
      print_space ();
      print_string "by";
      print_space ();
      print_string "SOL";
      print_space ();
      print_int (get_index h);
      print_newline ();
      dump cs h;
      print_newline ();
      close_box ()            
  | H_RPA (i, h) ->
      open_box 2;
      dump_clause i cs;
      print_space ();
      print_string "by";
      print_space ();
      print_string "RPA";
      print_space ();
      print_int (get_index h);
      print_space ();
      print_newline ();
      dump cs h;
      print_newline ();
      close_box ()                  
  | H_NEW i ->
      open_box 2;
      dump_clause i cs;
      print_space ();
      print_string "by";
      print_space ();
      print_string "INPUT";
      close_box ()                  
  | H_RES (i, h1, h2) ->
      open_box 2;
      dump_clause i cs;
      print_space ();
      print_string "by";
      print_space ();
      print_string "RES";
      print_space ();
      print_int (get_index h1);
      print_space ();
      print_string "into";
      print_space ();
      print_int (get_index h2);
      print_space ();
      print_newline ();
      dump cs h1;
      print_newline ();
      dump cs h2;
      print_newline ();
      close_box ()            
  | H_ESP (i, q) -> 
      open_box 2;
      dump_clause i cs;
      print_space ();
      print_string "by espilon-splitting";
      print_newline ();
      close_box ()            
;;



