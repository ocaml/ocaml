(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Damien Doligez, projet Para, INRIA Rocquencourt          *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

type stat = {
  minor_words : int;
  promoted_words : int;
  major_words : int;
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
  compactions : int
};;

type control = {
  mutable minor_heap_size : int;
  mutable major_heap_increment : int;
  mutable space_overhead : int;
  mutable verbose : bool;
  mutable max_overhead : int;
  mutable stack_limit : int
};;

external stat : unit -> stat = "gc_stat";;
external get : unit -> control = "gc_get";;
external set : control -> unit = "gc_set";;
external minor : unit -> unit = "gc_minor";;
external major : unit -> unit = "gc_major";;
external full_major : unit -> unit = "gc_full_major";;
external compact : unit -> unit = "gc_compaction";;

open Printf;;

let print_stat c =
  let st = stat () in
  fprintf c "minor_words: %d\n" st.minor_words;
  fprintf c "promoted_words: %d\n" st.promoted_words;
  fprintf c "major_words: %d\n" st.major_words;
  fprintf c "minor_collections: %d\n" st.minor_collections;
  fprintf c "major_collections: %d\n" st.major_collections;
  fprintf c "heap_words: %d\n" st.heap_words;
  fprintf c "heap_chunks: %d\n" st.heap_chunks;
  fprintf c "live_words: %d\n" st.live_words;
  fprintf c "live_blocks: %d\n" st.live_blocks;
  fprintf c "free_words: %d\n" st.free_words;
  fprintf c "free_blocks: %d\n" st.free_blocks;
  fprintf c "largest_free: %d\n" st.largest_free;
  fprintf c "fragments: %d\n" st.fragments;
  fprintf c "compactions: %d\n" st.compactions;
;;
