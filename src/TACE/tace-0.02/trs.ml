
open List;;

open Util;;
open Variable;;
open Term;;
open Equation;;


type trs = 
    equation list;;


let make () = 
  [];;


let add e (r : trs) = 
  let e1 = Equation.map_var (Variable.set_color 2) e
  in ((e1::r) : trs);;


let is_empty = function
    [] -> true
  | _ -> false;;


let length = 
  List.length;;


let map_var f = 
  List.map (Equation.map_var f);;


let map = 
  List.map;;


let rec to_string = 
  Util.list_to_string Equation.to_string;;


let check r = 
  List.for_all (Equation.for_all_var (fun x -> get_color x = 2)) r;;
