(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*            Damien Doligez, projet Para, INRIA Rocquencourt             *)
(*            Jacques-Henri Jourdan, projet Gallium, INRIA Paris          *)
(*                                                                        *)
(*   Copyright 1996-2016 Institut National de Recherche en Informatique   *)
(*     et en Automatique.                                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type stat = {
  minor_words : float;
  promoted_words : float;
  major_words : float;
  minor_collections : int;
  major_collections : int;
  heap_words : int;
  heap_chunks : int;
  live_words : int;
  live_blocks : int;
  free_words : int;
  free_blocks : int;
  largest_free : int;
  fragments : int;
  compactions : int;
  top_heap_words : int;
  stack_size : int;
  forced_major_collections: int;
}

type control = {
  minor_heap_size : int;
  major_heap_increment : int;
  space_overhead : int;
  verbose : int;
  max_overhead : int;
  stack_limit : int;
  allocation_policy : int;
  window_size : int;
  custom_major_ratio : int;
  custom_minor_ratio : int;
  custom_minor_max_size : int;
}

external stat : unit -> stat = "caml_gc_stat"
external quick_stat : unit -> stat = "caml_gc_quick_stat"
external counters : unit -> (float * float * float) = "caml_gc_counters"
external minor_words : unit -> (float [@unboxed])
  = "caml_gc_minor_words" "caml_gc_minor_words_unboxed"
external get : unit -> control = "caml_gc_get"
external set : control -> unit = "caml_gc_set"
external minor : unit -> unit = "caml_gc_minor"
external major_slice : int -> int = "caml_gc_major_slice"
external major : unit -> unit = "caml_gc_major"
external full_major : unit -> unit = "caml_gc_full_major"
external compact : unit -> unit = "caml_gc_compaction"
external get_minor_free : unit -> int = "caml_get_minor_free"
external eventlog_pause : unit -> unit = "caml_eventlog_pause"
external eventlog_resume : unit -> unit = "caml_eventlog_resume"

open Printf

let print_stat c =
  let st = stat () in
  fprintf c "minor_collections:      %d\n" st.minor_collections;
  fprintf c "major_collections:      %d\n" st.major_collections;
  fprintf c "compactions:            %d\n" st.compactions;
  fprintf c "forced_major_collections: %d\n" st.forced_major_collections;
  fprintf c "\n";
  let l1 = String.length (sprintf "%.0f" st.minor_words) in
  fprintf c "minor_words:    %*.0f\n" l1 st.minor_words;
  fprintf c "promoted_words: %*.0f\n" l1 st.promoted_words;
  fprintf c "major_words:    %*.0f\n" l1 st.major_words;
  fprintf c "\n";
  let l2 = String.length (sprintf "%d" st.top_heap_words) in
  fprintf c "top_heap_words: %*d\n" l2 st.top_heap_words;
  fprintf c "heap_words:     %*d\n" l2 st.heap_words;
  fprintf c "live_words:     %*d\n" l2 st.live_words;
  fprintf c "free_words:     %*d\n" l2 st.free_words;
  fprintf c "largest_free:   %*d\n" l2 st.largest_free;
  fprintf c "fragments:      %*d\n" l2 st.fragments;
  fprintf c "\n";
  fprintf c "live_blocks: %d\n" st.live_blocks;
  fprintf c "free_blocks: %d\n" st.free_blocks;
  fprintf c "heap_chunks: %d\n" st.heap_chunks


let allocated_bytes () =
  let (mi, pro, ma) = counters () in
  (mi +. ma -. pro) *. float_of_int (Sys.word_size / 8)


external finalise : ('a -> unit) -> 'a -> unit = "caml_final_register"
external finalise_last : (unit -> unit) -> 'a -> unit =
  "caml_final_register_called_without_value"
external finalise_release : unit -> unit = "caml_final_release"


type alarm = bool ref
type alarm_rec = {active : alarm; f : unit -> unit}

let rec call_alarm arec =
  if !(arec.active) then begin
    finalise call_alarm arec;
    arec.f ();
  end


let create_alarm f =
  let arec = { active = ref true; f = f } in
  finalise call_alarm arec;
  arec.active


let delete_alarm a = a := false

module Memprof =
  struct
    type allocation_source = Normal | Marshal | Custom
    type allocation =
      { n_samples : int;
        size : int;
        source : allocation_source;
        callstack : Printexc.raw_backtrace }

    type ('minor, 'major) tracker = {
      alloc_minor: allocation -> 'minor option;
      alloc_major: allocation -> 'major option;
      promote: 'minor -> 'major option;
      dealloc_minor: 'minor -> unit;
      dealloc_major: 'major -> unit;
    }

    let null_tracker = {
      alloc_minor = (fun _ -> None);
      alloc_major = (fun _ -> None);
      promote = (fun _ -> None);
      dealloc_minor = (fun _ -> ());
      dealloc_major = (fun _ -> ());
    }

    external c_start :
      float -> int -> ('minor, 'major) tracker -> unit
      = "caml_memprof_start"

    let start
      ~sampling_rate
      ?(callstack_size = max_int)
      tracker =
      c_start sampling_rate callstack_size tracker

    external stop : unit -> unit = "caml_memprof_stop"
  end
