(** Some utilities for lists and streams *)

open Stream

(***********************************)
(** {1 Lists misc}                 *)
(***********************************)

(** [tail_cons c l]
    @return the list obtained by adding [c] at the end of from [l]. *)
val tail_cons:
  'a -> 'a list -> 'a list


(** [nthtl l n]
    @return the pair made of the element of [l] at 
    position [n] (the first element of [l] is at position [0])
    and a list made from [l] where this element has been removed.
    @raise Invalid_arg id [n] is negative or is not the position
    of an element of [l]. *)
val nthtl:
  'a list -> int -> ('a * 'a list) 


(** [deoptionize l]
    @return the list containing all the elements [e]
    such that [Some e] is in [l] (in the same ordered). *)
val deoptionize:
  'a option list -> 'a list


(** [mapi f l]
    @return the the list of all elements 
    obtained by application of [f] to the pairs made of 
    an element of [l] together with its position in [l]
    (the first position is 0). *)
val mapi:
  ('a -> int -> 'b) -> 'a list -> 'b list


(** [multimap f [a1; ... ; an]]
    @return 
    [[a(1,1); a2;...; an]; ...; [a(1,k1); a2; ...; an];
     ...
    [a1; ...; an-1; a(n,1)]; ...; [a1; ...; an-1; a(n, kn)]]
    where [f(ai) = [a(i,1); ...; a(i, ki)]]

example:
    [multimap
   (fun x -> if x < 3 then [x+10; x+20] else [x+10; x+20; x+30]) 
   [1;2;3;4]]
 = 

    [
 [ [11; 2; 3; 4]; [21; 2; 3; 4]; 
   [1; 12; 3; 4]; [1; 22; 3; 4]; 
   [1; 2; 13; 4]; [1; 2; 23; 4]; [1; 2; 33; 4]; 
   [1; 2; 3; 14]; [1; 2; 3; 24]; [1; 2; 3; 34] ]
    ]  *)
val multimap:
  ('a -> 'a list) -> 'a list -> 'a list list


(** like {!Util.multimap} where [f: 'a -> ('a * 'b) list] 

example
  [ multimap2  
  (fun x -> 
     if x < 3 
     then [(x+10), 'a'; (x+20), 'b'] 
     else [(x+10), 'a'; (x+20), 'b'; (x+30), 'c'])    [1; 2; 3; 4]]
  =
  [
    [([11; 2; 3; 4], 'a'); ([21; 2; 3; 4], 'b'); 
    ([1; 12; 3; 4], 'a'); ([1; 22; 3; 4], 'b'); 
    ([1; 2; 13; 4], 'a'); ([1; 2; 23; 4], 'b'); ([1; 2; 33; 4], 'c'); 
    ([1; 2; 3; 14], 'a'); ([1; 2; 3; 24], 'b'); ([1; 2; 3; 34], 'c')] ] *)
val multimap2:
  ('a -> ('a * 'b) list) -> 'a list -> ('a list * 'b) list


(***********************************)
(** {1 Lists and equalities}       *)
(***********************************)

(** [meme eq a l] 
    @return true iff there exists an element equal to [a], wrt [eq], 
    in the list [l]. *)
val meme:
  ('a -> 'a -> bool) -> 'a ->  'a list -> bool


(** [clean eq l] 
    @return a copy of the list [l] where all doublons have 
    been eliminated. *)
val clean:
  ('a -> 'a -> bool) -> 'a list -> 'a list


(** [is_clean eq l] 
    @return true iff the list [l] has no repetition
    wrt [eq]. *)
val is_clean:
  ('a -> 'a -> bool) -> 'a list -> bool


(** [clean_ins eq a l] 
    @return a list made of the addition of [a] to the list [l] 
    if [a] does not belongs to [l], according to [eq],
    and [l] otherwise. *)
val clean_ins:
  ('a -> 'a -> bool) -> 'a -> 'a list -> 'a list


(** [clean_unions eq l1 l2] 
    @return the union of [l1] and [l2] 
    without repetition, wrt [eq], if [l1] and [l2] are without repetition. *)
val clean_union:
  ('a -> 'a -> bool) -> 'a list -> 'a list -> 'a list
  

(** [clean_rev_append eq l1 l2] 
    @return the concatenation of [l1] and [l2] where elements 
    of [l1] are added to [l2] without repetition. *)
val clean_rev_append:
  ('a -> 'a -> bool) -> 'a list -> 'a list -> 'a list


(** [list_equal eq l1 l2] 
    @return true iff the lists [l1] and [l2] contain
    the same elements according to [eq].
    
    [l1] and [l2] must be clean (without repetition). *)
val list_equal:
  ('a -> 'a -> bool) -> 'a list -> 'a list -> bool


(** [subseteq eq l1 l2]
    @return true iff every element of [l1] 
    is equal, wrt [eq] to an element of [l2]. *)
val subseteq:
  ('a -> 'a -> bool) -> 'a list -> 'a list -> bool


(** [list_meq eq l1 l2]
    multiset equality according to [eq]. *)
val list_meq:
  ('a -> 'a -> bool ) -> 'a list -> 'a list -> bool


(***********************************)
(** {1 Association lists}          *)
(***********************************)

(** [assoce eq a l] 
    @return the first value associated to a key equal to [a],
    wrt [eq], in the list of pairs [l].
    @raise Not_found if there is no value associated 
    to with a key equal to [a] in the list [l]. *)
val assoce:
  ('a -> 'a -> bool) -> 'a ->  ('a * 'b) list -> 'b 


(** [mem_assoce eq a l] 
    @return true iff there exists a value associated to a key equal to [a],
    wrt [eq], in the list of pairs [l]. *)
val mem_assoce:
  ('a -> 'a -> bool) -> 'a ->  ('a * 'b) list -> bool


(** [iassoc b l] 
    @return the first key to which [b] is associated in the list of pairs [l].
    i.e. [iassoc b [...; (a,b); ...] = a]
    if [(a,b)] is the leftmost binding to [b] in list [l].  
    @raise Not_found if there is no binding to [b] in the  list [l]. *)
val iassoc:
  'b -> ('a * 'b) list -> 'a


(** Same as {! Util.iassoc} but with physical equality to compare values. *)
val iassq:
  'b -> ('a * 'b) list -> 'a


(** [imem_assoc b l] 
    @return [true] if there is a binding [(a,b)]
    to [b] in the list of pairs [l].
    Return [false] otherwise. *)
val imem_assoc:
  'b -> ('a * 'b) list -> bool


(** Same as {! Util.imem_assoc} but with physical equality 
    to compare values. *)
val imem_assq:
  'b -> ('a * 'b) list -> bool
  

(***********************************)
(** {1 Int lists, positions}       *)
(***********************************)

(** [find_pos p l]
    @return [Some] of the first position of an element of [l] satisfying the
    predicate [p], or [None] if there are no such element in [l]. *)
val find_pos:
  ('a -> bool) -> 'a list -> int option


(** [find_allpos p l]
    @return the list of the positions of elements of [l] 
    satisfying the predicate [p]. *)
val find_allpos:
  ('a -> bool) -> 'a list -> int list


(** [replace l i a]
    @return the list obtained from [l]
    by replacement of the element at position [i] by [a]. 
    The first element of the list is at position [0].
    [l] must contain more than [i] elements. *)
val replace:
  'a list -> int -> 'a -> 'a list


(** [replacel l1 i l2]
    @return the list obtained from [l1]
    by replacement of the element at position [i]
    by the list [l2], or the concatenation 
    of [l1] and [l2] if [l1] is too short.
    The first element is at position [0]. *)
val replacel:
  'a list -> int -> 'a list -> 'a list


(** [remove l i]
    @return the list obtained by 
    removing the [i]th lement of [l].
    The first element is at position [0]. 
    @raise Failure if [l] is too short. *)
val remove:
  'a list -> int -> 'a list


(** [size_list f l]
    @return the sum of the respective sizes of the elements of the list [l],
    where the size of each element [a] is returned by [f a]. *)
val size_list:
  ('a -> int) -> 'a list -> int


(** [p_pplus n p]
    @return the list [[p,...,p+n-1]].  *)
val p_pplus:
  int -> int -> int list


(** [max l]
    @return the maximum of the integer list [l].
    @raise Invalid_argument if [l] is empty. *)
val max:
  int list -> int
    

(** [nthl l]
    @return the list [0,..., (length l - 1)].
    (alias for [{!Util.p_pplus} ({! List.length} l) 0]) *)
val nthl:
  'a list -> int list


(***********************************)
(** {1 Functions on booleans}      *)
(***********************************)

val xor:
  bool -> bool -> bool
  

(***********************************)
(** {1 Streams}       *)
(***********************************)


(** [is_empty st]
    @return [true] is the stream [st] is empty, [false] otherwise. *)
val is_empty:
  'a Stream.t -> bool


(** [surepeek st]
    @return (peek) the first element of the stream [st].
    @raise Failure if [st] is empty. *)
val surepeek:
  'a Stream.t -> 'a


(** [peek_nth n st]
    @return (peek) the [n]th element of the stream [st].
    The first element is at position 0.
    @raise Failure if [st] contains less than [n+1] elements. *)
val peek_nth:
  int -> 'a Stream.t -> 'a

(*****************************************)
(** {1 Strings and output} *)
(*****************************************)

(** [list_to_string f l]
    @return a string representation of the list [l],
    given a string conversion function [f] for the type
    of elements of [l]. 
    Every element [a] such that [f a] is the empty string is skipped. *)
val list_to_string:
  ('a -> string) -> 'a list -> string


(** [dump_list printer l]
    print the elements of the list [l]
    using the given printer [printer]. *)
val dump_list:
  ('a -> unit) -> 'a list -> unit

