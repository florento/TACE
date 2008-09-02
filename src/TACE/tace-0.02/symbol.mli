(** FUNCTION and predicate symbols, and precedence ordering. *)

(** Each symbol is made of a unique index,
    an arity, a kind, 4 flags, and a name (string). 

    Two symbols with same index are considered equal (see {! Symbol.equal}).

    The maximal number of symbols (maximal index value) is 65536.
    
    The maximal arity of a symbol is 255.   *)

open Stream
open Trace


(** type of symbol identifiers *)
type symbol


(** The precedence always respects the following ordering:
    [FUNCTION < SPLITTING < PREDICATE]  *)
type symbol_kind = 
    FUNCTION
      (** function symbols *)
  | PREDICATE
      (** predicate symbols from the initial specification *)
  | SPLITTING
      (** splitting predicate symbols introduced dynamically *)


(*************************)
(** {2 Reserved symbols} *)
(*************************)


(** Reserved symbol for term representation of lists, 
    see e.g. {! Substitution.to_term}.
    arity: [0], kind: [FUNCTION]. *)
val empty_symbol:
  symbol


(** Reserved symbol for term representation of lists, 
    see e.g. {! Substitution.to_term}.
    arity: [2], kind: [FUNCTION]. *)
val cons_symbol:
  symbol


(** Reserved symbol for term representation of variable bindings,
    see e.g. {! Substitution.to_term}.
    arity: [2], kind: [FUNCTION]. *)
val bind_symbol:
  symbol


(** Reserved symbol for equality.
    arity: [2], kind: [PREDICATE]. *)
val eq_symbol:
  symbol

(******************************************)
(** {2 Symbols creation and registration} *)
(******************************************)

(** [make name arity kind]
    @return the identifier of a new symbol,
    uniquely associated to the given string [name],
    [arity] and [kind].
    The string [name] is registered and
    the same id will be returned on
    calls to the function with the same string,
    unless arity ans kind differ (in this case, the function fail).

{b ATTENTION:} the symbols of each of the 3 kinds
    are (linearly) ordered, increasingly,
    by their order of registration.

@raise Failure if there are too many symbols 
    or arity is too big 
    or a symbol with the same [name] has alreay been registered
    with a different arity or different kind.*)
val make: 
  string -> int -> symbol_kind -> symbol


(** [of_string name arity]
    @param name string of:
    - ['a'..'w'] ['A'..'Z'|'a'..'z'|'_']* ['0'..'9']*   (kind [FUNCTION])
    - ['0'..'9']+    (kind [FUNCTION])
    - ['x'|'y'|'z'] ['A'..'Z'|'a'..'z'] ['A'..'Z'|'a'..'z'|'_']* ['0'..'9']*   
    (kind [FUNCTION])
    - ['A'..'Z'] ['A'..'Z'|'a'..'z'|'_']* ['0'..'9']*   
    (kind [PREDICATE])

    @return the identifier of a new symbol,
    uniquely associated to the given string [name] and given [arity].
    The string [name] is parsed 
    and registered with {! Symbol.make} and
    the same id will be returned on
    calls to the function with the same string,
    unless arity ans kind differ (in this case, the function fail).
    The kind of the symbol is deduced as in
    the above description of param [name].

    {b ATTENTION:} the symbols of each of the 3 kinds
    are (linearly) ordered, increasingly,
    by their order of registration.
    
    Note that it is not possible to register symbols
    of kind [SPLITTING] with this function. 
    {! Symbol.make} must be used for that purpose.

@raise Failure if there are too many symbols 
    or arity is too big 
    or a symbol with the same [name] has alreay been recorded
    with a different arity.
@raise Parsing.Parse_error also *)
val of_string: 
  string -> int -> symbol


(** Parse function or predicate symbol, unknown arity.
    [parse symbol ar st]
    @return the pair [(s, k)]
    where [s] is the symbol name (parsed string)
    and [k] is the kind of the parsed symbol, 
    [FUNCTION] or [PREDICATE].
    @raise Parsing.Parse_error if [st] does not contain a symbol
    (see {! Symbol.of_string}). *)
val parse_symbol:
  char Stream.t -> string * symbol_kind
  (* location starts with this symbol *)


(** [parse symbola ar st]
    parse function or predicate symbol, known arity.
    @return the parsed symbol, 
    registered with {! Symbol.make} with arity [ar].
    @raise Parsing.Parse_error see {! Symbol.of_string} for syntax. *)
val parse_symbola:
  int -> char Stream.t -> symbol


(** [is_registered id]
    @return [true] if the integer [id] is the identifier of
    a symbol registered with
  {! Symbol.make} or {! Symbol.of_string}. *)
val is_registered:
  symbol -> bool


(** [mem_symbol name]
    @return [true] if
    there is a symbol registered with the string [name],
    and [false] otherwise. *)
val mem_symbol: 
  string -> bool


(** [get_symbol name]
    @return the symbol registered with the string [name].
    @raise Not_found if no symbol was registered with the string [name]. *)
val get_symbol:
  string -> symbol


(** [nb_symbol ()]
    @return the current number of symbols registered. *)
val nb_symbols:
  unit -> int


(** [get_name id]
    @return the name of the given symbol [id] 
    @raise Not_found if no symbol has been registered with the given [id] *)
val get_name: 
  symbol -> string



(**************)
(** {2 Index} *)
(**************)

(** [get_index id]
    @return the index uniquely associated to the given symbol [id]. *)
val get_index: 
  symbol -> int


(**************)
(** {2 Arity} *)
(**************)

(** [get_arity id]
    @return the arity of the given symbol [id]. *)
val get_arity: 
  symbol -> int


(*************************)
(** {2 Kinds of symbols} *)
(*************************)

(** [get_kind id]
    @return the kind of the given symbol [id]. *)
val get_kind: 
  symbol -> symbol_kind


(** [string_of_kind k]
    @return a string corresponding to the kind [k]. *)
val string_of_kind:
  symbol_kind -> string


(** [int_of_kind k]
    @return an int value corresponding to the kind [k],
    - [1] for [FUNCTION]
    - [2] for [PREDICATE]
    - [3] for [SPLITTING]   *)
val int_of_kind:
  symbol_kind -> int


(**********************)
(** {2 Flags} *)
(**********************)

(** {4 Flag for blocked symbols} *)
(** One flag is used to mark symbol as blocked
    (for the basic strategy). *)


(** [block id]
    @return the given symbol [id] 
    with flag "blocked" set to [true]. *)
val block:
  symbol -> symbol


(** [unblock id]
    @return the given symbol [id] 
    with flag "blocked" set to [false]. *)
val unblock:
  symbol -> symbol


(** [is_blocked id]
    @return the value of the flag "blocked" for symbol [id]. *)
val is_blocked:
  symbol -> bool


(** {4 Flag for "test" predicate symbols} *)
(** One flag is used to mark predicate symbol as 
    "test" predicate symbols (for ordered strategy). *)


(** [set_testpredicate id]
    @return the given symbol [id] 
    with flag "test predicate" set to [true]. *)
val set_testpredicate:
  symbol -> symbol


(** [unset_testpredicate id]
    @return the given symbol [id] 
    with flag "test predicate" set to [false]. *)
val unset_testpredicate:
  symbol -> symbol


(** [is_testpredicate id]
    @return the value of the flag "test predicate" for symbol [id]. *)
val is_testpredicate:
  symbol -> bool



(** {4 All flags} *)
(** These functions should not be used, 
    to avoid conflicts,
    better define a new function as above. *)

(** [set_flag i id]
    @return the given symbol [id] with flag number [i] set to [true].
    [i] must be an [int] between [1] and [4].
    @raise Invalid_argument if [i < 1] or [i > 4]. *)
val set_flag:
  int -> symbol -> symbol


(** [set_flag i id]
    @return the given symbol [id] with flag number [i] set to [false].
    [i] must be an [int] between [1] and [4].
    @raise Invalid_argument if [i < 1] or [i > 4]. *)
val unset_flag:
  int -> symbol -> symbol


(** [get_flag i id]
    @return the value flag number [i] for symbol [id].
    [i] must be an [int] between [1] and [4].
    @raise Invalid_argument if [i < 1] or [i > 4]. *)
val get_flag:
  int -> symbol -> bool



(*****************************)
(** {2 Equality, inequality} *)
(*****************************)

(** [equal id1 id2]
    @return [true] if [id1] and [id2] have the same index
    return [false] otherwise. 

    The values of kind, arity, name and flags are
    ignored by this function.  *)
val equal:
  symbol -> symbol -> bool


(** [member s sl]
    @return true iff [s] occurs in [sl]. *)
val member:
  symbol -> symbol list -> bool

  (** [id1 << id2]
    precedence on symbols.
    - for symbols of different kinds, we always have:
      [FUNCTION << SPLITTING << PREDICATE]
    - for symbols of the same kind, the order
    is determined by the order of registration with 
      {! Symbol.make} or {! Symbol.of_string}. *)
val ( << ):
  symbol -> symbol -> bool

  

(***************)
(** {2 Output} *)
(***************)

(** [kind_to_string k]
      @return a string representation of the given symbol kind [k]. *)
val kind_to_string: 
  symbol_kind -> string


(** [to_string id]
      @return a string representation of the given symbol [id],
      i.e. its name.
    
    Moreover, 
    - if the flag1 is set, then '/1' is appened to the string representation,
    - if the flag2 is set, then '/2' is appened to the string representation, 
    - if the flag3 is set, then '/3' is appened to the string representation, 
    - if the flag4 is set, then '/4' is appened to the string representation. *)
val to_string: 
  symbol -> string


(** [dump id]
    print the internal representation of the symbol [id]. 
    Can be called on unregistered symbols ids. *)
val dump:
  symbol -> unit


(** [dump_symbols ()]
     print a list of registered symbols. *)
val dump_symbols:
  unit -> unit


(** [check v]
    @return true iff the variable [v] is conform to its definition. 
    For debug purposes. *)
val check:
  symbol -> bool


