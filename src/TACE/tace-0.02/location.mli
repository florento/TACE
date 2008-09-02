(** Locations of the elements in a source file *)


(** Type of locations. *)
type location =
{ 
  mutable file : string; 
  mutable start_line : int;
  mutable end_line : int;  
  mutable start_char : int;
  mutable end_char : int 
}


(** merge locations.
    [spawn l1 l2]
    @return a location with filename,
    starting line and column numbers as in the first given location [l1], 
    and with end line and column numbers as as in the second given 
    location [l2]. 
    @param l1 begin location
    @param l2 end location *)
val spawn: 
  location -> location -> location


(** pretty print the given location on standard output. *)
val print_location:
  location -> unit


(** [to_string loc]
    @return a formatted string describing the given location [loc]. *)
val to_string:
  location -> string


(** default location. *)
val default_location: 
  location
