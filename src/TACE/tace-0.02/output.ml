
open Trace;;
open Chrono;;
open Resolution;;
open Lparamodulation;;
open Solving;;
open Rparamodulation;;
open Subsumption;;
open Interpreter;;
open Saturation;;


let dump () = 
  let t_total = Chrono.read Saturation.monitor.sat_time in
  let t_resolution = Chrono.read Resolution.monitor.res_time in
  let t_paramodulation = 
    (Chrono.read Solving.monitor.sol_time) +.    
    (Chrono.read Rparamodulation.monitor.rpa_time) in
  let t_eager = Chrono.read Interpreter.monitor.interp_eager_time in
  let t_subsumption = Chrono.read Subsumption.monitor.sub_time in
  let n_sub = Subsumption.monitor.sub_tested in
  let n_sub_fail = n_sub - Subsumption.monitor.sub_success in
  let n_sub_filter = Subsumption.monitor.sub_fail_filter in
  let n_sub_filtered = 
    n_sub_filter.(0) + n_sub_filter.(1) + n_sub_filter.(2) + 
      n_sub_filter.(3) + n_sub_filter.(4) + n_sub_filter.(5) 
  in 
    trace 1 "Total time: %Fs (RES: %i%% PAR: %i%% EAG: %i%% SUB: %i%%)\n" 
      t_total 
      (int_of_float (t_resolution *. 100.0 /. t_total))
      (int_of_float (t_paramodulation  *. 100.0 /. t_total))
      (int_of_float (t_eager  *. 100.0 /. t_total))
      (int_of_float (t_subsumption *. 100.0 /. t_total));
    trace 5 "(RES: %Fs PAR: %Fs EAG: %Fs SUB: %Fs)\n" 
      t_resolution t_paramodulation t_eager t_subsumption;
    trace 1 "Nb of final clauses: %i (INIT: %i ADD: %i DEL: %i MAX_QUEUE: %i) \
%i iterations\n"
      Saturation.monitor.sat_clauses
      Saturation.monitor.sat_init
      Saturation.monitor.sat_added
      Saturation.monitor.sat_deleted      
      Saturation.monitor.sat_max_queue
      Saturation.monitor.sat_loop;
    trace 1 "Nb of subsumptions tested: %i failed: %i%% filtered: %i%% \
(%i%%, %i%%, %i%%, %i%%, %i%%, %i%%)\n"
      n_sub
      (n_sub_fail * 100 / n_sub)
      (n_sub_filtered * 100 / n_sub_fail)      
      (n_sub_filter.(0) * 100 / n_sub_filtered)
      (n_sub_filter.(1) * 100 / n_sub_filtered)
      (n_sub_filter.(2) * 100 / n_sub_filtered)
      (n_sub_filter.(3) * 100 / n_sub_filtered)
      (n_sub_filter.(4) * 100 / n_sub_filtered)
      (n_sub_filter.(5) * 100 / n_sub_filtered);;
      
    
