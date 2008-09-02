
open Printf;;
open Trace;;

set_trace_level 6;;


let start_test () = 
  debug_off ();
  cls ();
  set_cursor 1 2;
  printf "Premiere ligne: \n";
  printf "Deuxieme ligne: \n";
  set_cursor 18 2;
  printf "AAA";
  set_cursor 18 3;
  printf "HHH";
  set_cursor 18 2;
  printf "BB";
  set_cursor 18 3;
  printf "II";
  set_cursor 18 2;
  printf "C";
  set_cursor 18 3;
  printf "J";
  set_cursor 1 5;
  printf "%!\n";;

    
