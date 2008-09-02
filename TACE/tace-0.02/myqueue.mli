(** Priority queues *)


(** Queue of elements of type ['a]. *)
type 'a queue


(** raised when {! Myqueue.next} is applied to an empty queue. *)
exception Empty


(** [empty ()]
    @return an empty queue. *)
val make:
  unit -> 'a queue


(** [is_empty q]
    @return true iff [q] is empty. *)
val is_empty:
  'a queue -> bool


(** [length q]
    @return the number of elements in [q]. *)
val length:
  'a queue -> int


(** [add c q ord eq]
    adds the element [c] to the queue [q], 
    using the prority partial ordering [ord],
    if [c] is not already in [q] wrt the equality [eq]. *)
val add:
  'a -> 'a queue -> ('a -> 'a -> bool) -> ('a -> 'a -> bool) -> unit

  
(** [next q]
    @return an element of [c] with maximal priority 
    and removes it from [q].
    @raise Empty if [q] is empty. *)
val next:
  'a queue -> 'a


(** [dump q pr]
    print the contents of the clausequeue [q], using 
    the printer [pr] for the elements of type ['a]. *)
val dump:
  'a queue -> ('a -> unit) -> unit
