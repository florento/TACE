
open Trs
open Clause



(** [import_clauses cl]
    @return a pair made of 
    - a trs which contains the equations of positive equational clauses 
    in the list of clauses [cl] , 
    - a list of the other clauses of [cl].
    
    The clauses are preprocessed:
    - mark test predicate *)
val import_clauses:
  clause list -> trs * clause list


(** [import_file filename]
    @return the application of {! Input.import_clauses} to the
    clauses parsed in [filename].
    Every clause is parsed with {! Clause.of_string}.
    A comment is a line starting with '#' (with optional spaces). 
    Clause are preprocessed as in {! Import.import_clauses} 
    before being returned. *) 
val import_file:
  string -> trs * clause list


