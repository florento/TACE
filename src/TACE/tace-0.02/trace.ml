open Printf;;

exception Internal_error of string;;

let no_trace = 0;;
let max_trace = 5;;
let default_trace = 1;;

let debug_mode = ref false;;
let trace_level = ref default_trace;;


(** print on the standard output the concatenation 
  of the given string and of the given formatted message. 

  The formatted messages [format arg1 ... argn] 
  has the same form than for [printf].

  @see <http://caml.inria.fr/ocaml/htmlman/libref/Printf.html> 
  the Ocaml module Printf 

 Colors codes:
 - BLACK        0
 - RED           1
 - GREEN       2
 - YELLOW      3
 - BLUE          4
 - MAGENTA  5
 - CYAN         6
 - WHITE        7
 
 Attributes codes:
 - RESET         0 Reset All Attributes (return to normal mode) 
 - BRIGHT       1 Usually turns on BOLD 
 - DIM             2
 - UNDERLINE  3
 - BLINK          4
 - REVERSE      7
 - HIDDEN       8 
*)

let internal_print prefix fmt =
  let k result = print_string (prefix^result);
  in kprintf k fmt;;


(** prints on the given output channel the concatenation 
  of the given string and of the given formatted message. *)
let internal_fprint ch prefix fmt =
  let k result = fprintf ch "%s" (prefix^result);
  in kprintf k fmt;;


(** Do nothing. *)
let ignore fmt =
  kprintf (fun x -> ()) fmt;;


let debug_on () = 
  debug_mode := true;;


let debug_off () = 
  debug_mode := false;;


let get_debug () = 
  !debug_mode;;


let set_trace_level l = 
  if l <= no_trace
  then 
    trace_level := no_trace
  else if l >= max_trace
  then 
    trace_level := max_trace
  else 
    trace_level := l;;


let trace_off () = 
  trace_level := no_trace;;


let get_trace_level () = 
  !trace_level;;


let trace l fmt =
  if ( !trace_level >= l )
(*  then if ( l <= 4 ) *)
(*  then internal_fprint stdout "\x1b[0;30;42mTRACE:  \x1b[0m  " fmt *)
  then internal_fprint stdout "\x1b[0;37;42mTRACE:  \x1b[0m  " fmt
  else ignore fmt;;


let internal_error fmt =
  internal_fprint stdout "\x1b[0;37;41mERROR:  \x1b[0m  " fmt;;


let error fmt = 
  internal_fprint stdout "\x1b[0;37;41mERROR:  \x1b[0m  " fmt;;


let internal_fprint_true ch prefix fmt =
  kprintf (fun s -> true) fmt;;


let internal_fprint_false ch prefix fmt =
  kprintf (fun s -> begin (fprintf ch "%s" (prefix^s)); false end) fmt;;


let check b = 
  if b
  then kprintf (fun s -> true)
  else kprintf 
    (fun s -> 
       begin 
	 (print_string ("\x1b[0;37;41mERROR:  \x1b[0m  "^s)); 
	 false 
       end);;


let warning fmt = 
  internal_print "\x1b[0;31;43mWARNING:\x1b[0m  " fmt;;


let test fmt = 
  internal_print "\x1b[0;36;44mTEST:   \x1b[0m  " fmt;;


let debug fmt = 
  if !debug_mode
  then internal_print "\x1b[0;30;42mDEBUG:  \x1b[0m  " fmt
  else ignore fmt;;


let debug_b fmt = 
  if !debug_mode
  then 
    kprintf (fun s -> print_string ("\x1b[0;30;42mDEBUG:  \x1b[0m  "^s); true)
      fmt
  else kprintf (fun s -> true) fmt;;  


let set_cursor x y =
  if x <= 0 then (if y > 0 then printf "\x1b[%id" y)
  else (* x > 0 *) if y <= 0 then printf "\x1b[%iG" x
  else printf "\x1b[%i;%iH" y x


let cls () = 
  printf "\x1b[2J";;
