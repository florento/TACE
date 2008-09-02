(** Variables manipulation. *)

(**
   Every variable is associated with:
   - an index (int smaller than 16777215), 
   - a name (string) and 
   - an optional color (int between 0 and 15, 0 meaning without color).
   
   A variable can be allocated either with 
   {! Variable.of_string} or {! Variable.make}
   (see the definitions of these functions for the index, 
   names, and colors of allocated variables).
   
   Variables equality is physical equality.
   Two variables are equal if they have the same index, name,
   and color (see {! Variable.equal}).  *)
type variable


(** a special variable different from all
    variables registered with {! Variable.make}
    or {! Variable.of_string}. *)
val var0:
  variable 


(** [of_string s]
    @param s a string of the form ['x' 'y' 'z'] ['0'..'9']*
    @return a variable uniquely associated to the given string [s] (parsed),
    without color.
    If the function is called twice on the same string,
    the same variable is returned.
    The variable is guaranted to be different from any variable allocated with
    {! Variable.make}.
    @raise Variable.Parse_error [i] where [i] is the position 
    of the error in the string [s]. *) 
val of_string:
  string -> variable


(** [parse_variable st]
    @return the variable with name parsed from the char stream [st],
    registered if it is a new variable.
    @raise Parsing.Parse_error [i]. *)
val parse_variable:
  char Stream.t -> variable


(** [make i]
    @return a variable with index [i], with name ['#i'] and without color.
    The variable is guaranted to be different from any variable allocated with
    {! Variable.of_string}.
    @raise Failure if [i] is bigger than 16777215. *)
val make:
  int -> variable


(** [set_color i v]
    @return the variable [v] with color set to [i]. *)
val set_color:
  int -> variable -> variable
 

(** [decolor v]
    @return the variable [v] without color. *)
val decolor:
  variable -> variable


(** [get_color v]
    @return the color of the variable [v]. *)
val get_color:
  variable -> int


(** [get_index v]
    @return the index of the variable [v]. *)
val get_index:
  variable -> int


(** [equal v1 v2]
    @return [true] if the variables [v1] and [v2]
    have the same index, name and color value, 
    return [false] otherwise. *)
val equal:
  variable -> variable -> bool


(** [pairwise_distinct vl]
    @return [true] if the variables of the list [vl] 
    are pairwise disjoint,
    return [false] otherwise. *)
val pairwise_distinct:
  variable list -> bool


(** [check v]
    @return true iff the variable [v] is conform to its definition. *)
val check:
  variable -> bool


(** [to_string v]
    @return a string representation of the variable [v],
    which is the concatenation of the name of [v] 
    and  ['!'c] if [v] has a color [c > 0].
    - if [v] has been returned by a call [of_string s],
    its name [s],
    - otherwise, its name is ['#'i], where [i] is its index.

    @raise Failure if [i] is not a variable.*)
val to_string:
  variable -> string


(** [dump v]
    print the internal representation of the variable [v].  *)
val dump:
  variable -> unit


(** [dump_vars ()]
    print the list of registered variables with internal representation.  *)
val dump_vars:
  unit -> unit
