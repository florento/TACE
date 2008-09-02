
open Sys;;

open Trace;;


type chrono = {
  ch_id : string;
  (* the system time at the last start. *)  
  mutable ch_start : float;
  (* the time counted between the first start and the last stop.
     We do not count time between a stop and a start.  *)
  mutable ch_total : float;
  (* true iff the chrono is started *)
  mutable ch_run : bool;
  (* true iff the chrono has ever been started after creation
     or last reset. *)
  mutable ch_started : bool;
};;


let make name = 
  assert (debug_b "Chrono.make\n");
  {
    ch_id = name;
    ch_start = 0.0;
    ch_total = 0.0;
    ch_run = false;
    ch_started = false;
  };;


let start c = 
  if c.ch_run
  then 
    warning "Chrono.start %s: already running\n" c.ch_id
  else begin
    c.ch_start <- Sys.time ();    
    c.ch_run <- true;
    c.ch_started <- true
  end;;


let stop c = 
  if c.ch_run
  then begin
    let current = Sys.time () in
      c.ch_total <- c.ch_total +. (current -. c.ch_start);
      c.ch_start <- 0.0;
      c.ch_run <- false;
  end
  else  
    warning "Chrono.stop %s: not running\n" c.ch_id;;


let reset c = 
  c.ch_start <- 0.0;
  c.ch_total <- 0.0;
  c.ch_run <- false;
  c.ch_started <- false;;


let is_running c = 
  c.ch_run;;


let read c = 
  if not c.ch_started
  then begin
    warning "Chrono.read %s: not started\n" c.ch_id; 
    0.0;
  end
  else if c.ch_run
  then 
    c.ch_total +. ((Sys.time ()) -. c.ch_start)
  else 
    c.ch_total;;










