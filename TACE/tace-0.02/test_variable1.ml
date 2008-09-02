
open Printf;;
open Trace;;
open Variable;;

set_trace_level 6;;


let display_var v =
  (printf "var id: %i, name: %s, color: %i, dump:" 
     (get_index v)
     (to_string v) 
     (get_color v)); 
  Variable.dump v;
  printf "\n";;
  


let start_test () = 
  let z1 = (make 1) in
  let z2 = (set_color 12 (make 2)) in    
  let x1 = (of_string "x1") in
  let x2 = (set_color 10 (of_string "x2")) in
  let x3 = (of_string "x1") 
  in begin
      dump_vars ();
      display_var z1;
      display_var z2;
      display_var x1;
      display_var x2;
    end
;;

