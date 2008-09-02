(** proofs *)

open Symbol
open Clause
open Clauseset


type history =
    H_NIL of int
  | H_LPA of int * history
  | H_SOL of int * history
  | H_RPA of int * history
  | H_NEW of int
  | H_RES of int * history * history
  | H_ESP of int * symbol


(** [of_clauseset cs i]
    @return the history of the construction of the clause at position
    [i] in the clauseset [cs]. *)
val of_clauseset:
  clauseset -> int -> history


(** [dump h]
    print the contents of the history [h]. *)
val dump:
  clauseset -> history -> unit


