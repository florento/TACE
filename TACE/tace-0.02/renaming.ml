
open List;;

open Variable;;
open Term;;


type renaming = {
  mutable re_map : (variable * variable) list;
  mutable re_cpt : int
};;


let make () = 
  { 
    re_map = [];
    re_cpt = 1
  };;


let rec mem_binding x = function
    [] -> false
  | (y,z)::l -> 
      if (Variable.equal x y) 
      then true 
      else mem_binding x l;;


let mem x ren = 
  mem_binding x ren.re_map;;
  

let rec get_binding x = function
    [] -> raise Not_found
  | (y,z)::l -> 
      if (Variable.equal x y) 
      then z 
      else get_binding x l;;


let apply ren x = 
  if (mem x ren)
  then get_binding x ren.re_map
  else 
    let z = (Variable.make ren.re_cpt) in 
      begin
	ren.re_map <- (x,z)::ren.re_map;
	ren.re_cpt <- ren.re_cpt + 1;
	z
      end;;


