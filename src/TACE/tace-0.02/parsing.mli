(** Various parsing functions for data structures *)

open Stream
open Location


exception Parse_error of string * location;;


(** [make_parse_error msg]
    @return a new {! Parsing.Parse_error}
    built with the given message (string) [msg]
    and the current location. *)
val parse_error:
  string -> exn


(** a global {i current location} is maintained in the module,
    and is returned by {! Parsing.parse_error}
    (with the exception {! Parsing.Parse_error}.
    It is updated automatically by the functions
    {! Parsing.nextchar},
    {! Parsing.skip} and {! Parsing.scan},
    and it can be updated manually with 
    {! Parsing.reset_location}
    and {! Parsing.restart_location}.
    It is returned by {! Parsing.get_location}. *)    


(** [get_location ()]
    @return the durrent location. *)
val get_location:
  unit -> location


(** reset the current location to first line, first char *)
val reset_location:
  unit -> unit


(** [restart_location ()]
    @return change the value of the current location,
    making it start
    where the current location ends. *)
val restart_location:
  unit -> unit


(** set the filename of the current location. *)
val set_filename:
  string -> unit


(** [get_filename ()]
    @return the filename of the current location. *)
val get_filename:
  unit -> string


(** [nextchar st]
    @return the first element of the stream of char [st].
    @raise Stream.Failure if the stream [st] empty *)
val nextchar:
  char Stream.t -> char


(** [skip st]
    remove the first element of the char stream [st]. *)
val skip:
  char Stream.t -> unit


(** [scan c st]
    @return [()] if the first element of the char stream [st] is [c]. 
    This element is removed from [st].
    @raise Parse_error otherwise. *)
val scan:
  char -> char Stream.t -> unit
  

