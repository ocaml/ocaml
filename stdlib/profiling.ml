(***********************************************************************)
(*                                                                     *)
(*                         Caml Special Light                          *)
(*                                                                     *)
(*      Damien Doligez and Francois Rouaix, INRIA Rocquencourt         *)
(*   Ported to Caml Special Light by John Malecki and Xavier Leroy     *)
(*                                                                     *)
(*  Copyright 1995 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Run-time library for profiled programs *)

type profiling_counters = (string * (string * int array)) list

let counters = ref ([] : profiling_counters)

exception Bad_profile

let dump_counters () =
  begin try 
    let ic = open_in_bin "cslprof.dump" in
    let prevl = (input_value ic : profiling_counters) in
    close_in ic;
    List.iter2
      (fun (curname, (curmodes,curcount)) (prevname, (prevmodes,prevcount)) ->
        if curname <> prevname
        or curmodes <> prevmodes
        or Array.length curcount <> Array.length prevcount
        then raise Bad_profile)
      !counters prevl;
    List.iter2
      (fun (curname, (_,curcount)) (prevname, (_,prevcount)) ->
        for i = 0 to Array.length curcount - 1 do
          curcount.(i) <- curcount.(i) + prevcount.(i)
        done)
      !counters prevl
  with _ -> ()
  end;
  begin try
    let oc = open_out_bin "cslprof.dump" in
    output_value oc !counters;
    close_out oc
  with _ -> ()
  end

let _ = at_exit dump_counters

