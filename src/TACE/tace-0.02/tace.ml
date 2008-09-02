(** main module baby *)

open Printf;;
open Format;;
open Sys;;
open Filename;;

open Trace;;
open Util;;
open Symbol;;
open Parsing;;
open Clauseset;;
open History;;
open Saturation;;
open Input;;
open Output;;


let short_usage () =
  print_string "Usage: tace [options] file"; 
  print_newline ();
  print_string "tace -h  for options";
  print_newline ();
  exit 1;;


let usage () =
  print_string "Usage: tace [options] file"; 
  print_newline ();
  print_string "options:";
  print_newline ();
  open_box 2;
  print_string "-c or --continue : continue saturation when empty clause found\
(default: halt)";
  print_newline ();
  print_string "-q or --clause_queue : store clauses to be proceeded in a queue\
(default: store instructions only)";
  print_newline ();
  print_string "-t l or --trace l: trace level";
  print_newline ();
  print_string " level 0 : no trace at all,\
 just display the output at the end of computation";
  print_newline ();
  print_string " level 1 : \
display only the number of clauses and heap size etc";
  print_newline ();
  print_string " level 2 : main intermediate stages";
  print_newline ();
  print_string " level 3 : all deduction steps (non-eager inferences)";
  print_newline ();
  print_string " level 4 : all deduction steps \
(eager and non-eager inferences)";
  print_newline ();
  print_string " level 5 : all messages";
  print_newline ();
  print_string " default : 1";
  print_newline ();
  print_string "-d or --debug : debugging output on (default off)";
  print_newline ();
  print_string "-h or --help  : this message";
  print_newline ();
  close_box ();
  exit 1;;


let fail name msg cs = 
  error "%s: %s\n" name msg;;


let queue_mode = ref QUEUE_INSTR;;

let halt_mode = ref HALT_FIRST;;

let filename = ref "";;

let rec parse_option args i =
  printf "parse option %i\n" i;
  if i = ((Array.length args)-1) 
  then 
    (* help and no other option *)
    if (args.(i) = "-h") || (args.(i) = "--help")
    then parse_help args i
    (* filename and no option *)
    else parse_filename args i
  else let arg = args.(i) in
    if (arg = "-c") || (arg = "--continue")
    then parse_continue args i
    else if (arg = "-q") || (arg = "--clause_queue")
    then parse_queue args i
    else if (arg = "-d") || (arg = "--debug")
    then parse_debug args i
    else if (arg = "-h") || (arg = "--help")
    then parse_help args i
    else if (arg = "-t") || (arg = "--trace")
    then parse_level args (i+1)
    else if ((String.get arg 0) = '-')
    then begin 
      (error "unknown option %s\n" arg); 
      printf "unknown option %s\n" arg;
      short_usage () 
    end
and parse_debug args i = 
  printf "parsed option -d: DEBUG\n";
  debug_on ();
  parse_option args (i+1)
and parse_help args i = 
  printf "parsed option -h: HELP\n";
  usage ()
and parse_continue args i =
  printf "parsed option -c: HALT_CONTINUE\n";
  halt_mode := HALT_CONTINUE;
  parse_option args (i+1)
and parse_queue args i =
  printf "parsed option -q: queue_mode = QUEUE_CLAUSE\n";
  queue_mode := QUEUE_CLAUSE;
  parse_option args (i+1)
and parse_level args i = 
  printf "parsed trace level %s\n" args.(i);
  try 
    set_trace_level (int_of_string args.(i));
    parse_option args (i+1)
  with
      Failure _ -> 
	printf "failure\n";
	short_usage ()
and parse_cmd args i = 
  printf "parsed cmd %s\n" args.(i);
  if (basename args.(i)) <> "tace"
  then begin
    printf "wrong command name %s\n" (basename args.(i));
    short_usage ()
  end
  else 
    parse_option args (i+1)
and parse_filename args i = 
  printf "parsed filename %s\n" args.(i);
  let arg = args.(i) in
    if ((String.get arg 0) = '-')
    then begin
      printf "filename expected\n";
      short_usage ();
    end
    else 
      Parsing.set_filename arg;;




(** main *)
let _ =
  let args = Sys.argv in
    (* commandline must have at least 1 param *)
    if ( (Array.length args) < 2 )
    then begin
      printf "arguments missing\n";
      debug "only %i arguments\n" (Array.length args);
      short_usage ();
    end;
    (* parse command line arguments *)    
    parse_cmd args 0;
    let filename = Parsing.get_filename () in
      (* no filename given *)
      if filename = "" 
      then begin
	print_string "error: no filename given.\n";
        (* there must be a filename *)
	short_usage ();
      end
      else if not (Sys.file_exists filename)
      then 
        begin 
          error "%s: file not found\n" filename; 
          exit 3; 
        end;
      (* parse *)
      trace 2 "parse file %s\n" filename;       
      let (r, cl) = Input.import_file filename in
      let cs = Clauseset.make r 
      in try
	  debug "file %s parsed\n" filename;
	  debug "symbols\n";
	  if !debug_mode
	  then dump_symbols ();
	  debug "trs: %s\n" (Trs.to_string r);
	  debug "initial clauses\n";
	  if !debug_mode
	  then List.iter
	    (fun c -> debug "  %s\n" (Clause.to_stringt c)) cl;
	  print_newline ();
	  debug "start saturation...\n";
	  let hl = saturate cs cl !queue_mode !halt_mode in	    
	    (* print time and  stats *)
	    Output.dump ();
	    (* print result of saturation *)
	    if (hl = [])
	    then begin
	      trace 0 "No Proof found\n";	    
	      if (get_trace_level () >= 3)
	      then begin
		trace 2 "final set of clauses\n";
		Clauseset.dump cs;
	      end;
	    end
	    else 
	      begin
		trace_off ();
 		trace 0 "%i proofs found:\n" (List.length hl);
		(* TBD display all proofs, not only the first *)
		List.iter 
		  (fun h -> print_string "proof:\n"; 
		     History.dump cs h; 
		     print_newline ()) hl;
		Clauseset.dump cs;
	      end;
	with
	    Failure msg -> 
	      fail "Failure" msg cs;
	  | Assert_failure (file, line, col) ->
	      fail "Assert_failure" 
		(sprintf "%s: line %i char %i" file line col) cs
	  | Invalid_argument msg ->		
	      error "Invalid_argument %s\n" msg;;	


