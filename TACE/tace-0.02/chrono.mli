(** Chronometers *)


type chrono


(** [make s]
    Return a new chronometer with name [s], set to [0.0], not started. *)
val make:
  string -> chrono


(** [start c]
    Start the chrono [c]. *)
val start:
  chrono -> unit


(** [stop c]
    Stop the chrono [c]. It is not reset to 0
    and keeps its value until it is restarted. *)
val stop:
  chrono -> unit


(** [reset c]
    Set the chrono [c] to 0. *)
val reset:
  chrono -> unit


(** [read c]
    @return the number of seconds elpased since the chrono [c]
    was started. *)
val read:
  chrono -> float


(** [is_running c]
    @return whether the chrono [c] is running
    (the last action among start, stop, reset is start). 
    After creation with {! Chrono.make}, a chrono is not running. *)
val is_running:
  chrono -> bool
