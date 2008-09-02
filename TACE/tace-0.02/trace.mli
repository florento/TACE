(** Internal functions for trace and errors *)


(** raised for debug purpose to report an unexpected error,
  other than an error in the input file. *)
exception Internal_error of string;;


(* val internal_error: ('a, unit, unit) format -> 'a;; *)

(** [set_trace_level level]
    set trace level to [level].
    The values for [level] are 
    - [0]: no trace at all, just display the output at the end of computation
    - [1]: display only the number of clauses and heap size etc
    - [2]: main intermediate stages
    - [3]: all deduction steps (non-eager inferences)
    - [4]: all deduction steps (eager and non-eager inferences)
    - [5]: all messages

    If [level] is higher than [5], the trace level is set to [5].
    If [level] is negative, the trace level is set to [0].  *)
val set_trace_level: 
  int -> unit


(** [trace_off ()]
    set trace level to [0]. *)
val trace_off: 
  unit -> unit


(** [get_trace_level () ]
    @return the current tracel level.
    The default trace level is [1]. *)
val get_trace_level: 
  unit -> int


(** [debug_on ()]
    enter debug mode. *)
val debug_on:
  unit -> unit


(** [debug_off ()]
    quit debug mode. *)
val debug_off:
  unit -> unit


(** flag of debug mode *)
val debug_mode:
  bool ref


(** [get_debug ()]
    @return true iff in the debug mode. *)
val get_debug:
  unit -> bool

(** [trace level format arg1 ... argn]
    Print the given formatted message on the std output,  
    if the current trace level is equal to or higher than [level]. 
    Otherwise do nothing. *)
val trace: 
  int -> ('a, unit, string, unit) format4 -> 'a


(** [internal_error format arg1 ... argn]
    report the given formatted error message on std output.
    Use for system errors. *)
val internal_error: 
  ('a, unit, string, unit) format4 -> 'a


(** [error format arg1 ... argn]
    report the given formatted error message on std output.
    Use for user errors. *)
val error: 
  ('a, unit, string, unit) format4 -> 'a


(** [check b format arg1 ... argn]
    @return: [b] and moreover, 
    if [b] is [false] then 
    the given formatted error message 
    is reported on std output. *)
val check: 
  bool -> ('a, unit, string, bool) format4 -> 'a


(** [error format arg1 ... argn]
    report the given formatted warning message on std output.
    Use for user warnings. *)
val warning:
  ('a, unit, string, unit) format4 -> 'a


(** [test format arg1 ... argn]
    report the given formatted warning message on std output.
    Use for tests. *)
val test:
  ('a, unit, string, unit) format4 -> 'a


(** [debug format arg1 ... argn]
    report the given formatted debug message on std output
    only if the debug mode is on. *)
val debug:
  ('a, unit, string, unit) format4 -> 'a


(** [debug_b format arg1 ... argn]
    same as {! Trace.debug} but returns true.

    To be used with [assert] to avoid compilation
    of debug code. *)
val debug_b:
  ('a, unit, string, bool) format4 -> 'a


(** [set_cursor x y]
    set the cursor at line [y] 
    and character [x] ) of the screen.
    The first line is [1], 
    the first character is [1]. *)
val set_cursor:
  int -> int -> unit


(** [cls ()]
    clear the terminal screen. *)
val cls:
  unit -> unit
