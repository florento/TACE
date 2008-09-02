(* Main loop *)

open Chrono
open Clause
open Clauseset
open History



(** We implement 2 versions of queues of clauses to be proceed:
    - [QUEUE_INSTR] 
    pairs instruction * size of clause returned by instruction.
    - [QUEUE_CLAUSE] 
    pairs clause * instruction returning the clause. *)
type queuemode = 
    QUEUE_INSTR 
  | QUEUE_CLAUSE


(** We implement 2 halting conditions:
    - [HALT_FIRST] halt computation as soon as the empty clause is produced
    - [HALT_CONTINUE] continue up to saturation. *)
type haltmode = 
    HALT_FIRST 
  | HALT_CONTINUE;;


(** monitor of the resolution module *)
type monitor = {
  mutable sat_loop      : int;
  (** number of loop execution *)
  mutable sat_init     : int;
  (** number of initial clauses. *)
  mutable sat_clauses   : int;
  (** total number of clauses in the saturated clauseset.. *)
  mutable sat_added     : int;
  (** number of clause added to the saturated clauseset. *)
  mutable sat_deleted   : int;
  (** number of resolution steps which failed after filters. *)
  mutable sat_max_queue : int;
  (** maximal length reached by the clause queue during saturation *)
  mutable sat_time      : chrono;
  (** chronometer of the total time spent in saturation. *)
}


(** global monitor, uptated by the module *)
val monitor : monitor


(** [saturate cs cl queue_mode halt_mode]
    @return the list of histories of derivation of the empty clause
    from [cl] and the [cs].

    The clauseset [cs] is assumed saturated (no interaction between the
    clauses of [cs] is computed.
    The clauseset [cs] may contain an equational theory. *)
val saturate:
  clauseset -> clause list -> queuemode -> haltmode -> history list
  

(** {b Description of the saturation loop}
    2 set of clauses:
    - a {! Clauseset.clauseset} of saturated clauses
    - a queue to be treated (type {! Myqueue.queue})
    which contains either:
    - queue of clauses,
    - queue of instructions.

    Saturation loop, according to the elements in queue:


{v CLAUSE queue                           INSTR queue                         v}
{v                                                                            v}
{v LOOP                                   LOOP                                v}
{v - queue.next = 1 clause                - queue.next = 1 instruction        v}
{v                                        - exec instructioh = clause list    v} 
{v - eager inferences = clause list       - eager inferences = clause list    v}
{v                                                                            v}
{v - forward subsumption                  - forward subsumption               v}    
{v - add cs  all clause of list           - add cs  all clauses of list       v}
{v - backward subsumption                 - backward subsumption              v}
{v - (non-eager) inferences               - (non-eager) inferences            v}
{v   = clause list                        = clause list                       v}
{v                                                                            v}
{v - add queue the clauses obtained       - add queue the corr. instructions  v}
{v                                                                            v}
{v ENDLOOP                                ENDLOOP                             v}      

    non-eager inferences:
    - resolution
    - right-paramodulation
    - left-paramodulation = eq. solving

    eager inferences:
    - tautology elim
    - trivial lit. elim.
    - e-splitting
*)



(** OBSOLETE {b Initial clauses.}
    The saturation is performed by execution of {! Instruction}s. 
    
    We assume that initially, the clause list contains clauses of the form:
    - positive equational clauses [=> s = t], transfered into the {! Trs},
    - regular clauses (see {! Rclause}), of kind [0] or [1].
    Intially not saturated, 
    and treated with instructions {! Instruction}.[GET],
    - equational clauses [Q1(x1),...,Qn(xn), e => Q(x)] of kind [2],
    where [e] is a non empty list of equations.
    Treated with {! Instruction}.[NRW] wrt to the trs of the [clauseset]
    (until [e] is empty),
    - goal clauses [Q(t) =>]. *) 
