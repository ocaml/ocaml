(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2015--2016 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module Position : sig

  type t

  val filename : t -> string
  val line_number : t -> int
  val start_char : t -> int
  val end_char : t -> int

  val compare : t -> t -> int
  val hash : t -> int

end

module Location : sig

  type t

  val address : t -> Int64.t
  val symbol : t -> string option
  val position : t -> Position.t option
  val foreign : t -> bool

  val compare : t -> t -> int
  val hash : t -> int

end

module Backtrace : sig

  type t = Location.t list

  val compare : t -> t -> int
  val hash : t -> int

end

module Entry : sig

  type t

  val backtrace : t -> Backtrace.t
  val blocks : t -> int
  val words : t -> int

  val compare : t -> t -> int
  val hash : t -> int

end

module Entry_sorted_by_words_highest_first : sig
  type t = Entry.t
  val compare : t -> t -> int
  val hash : t -> int
end

module Entries : Set.S
  with type elt = Entry.t
  and type t = Set.Make(Entry).t

module Entries_sorted_by_words_highest_first : Set.S
  with type elt = Entry.t
  and type t = Set.Make(Entry_sorted_by_words_highest_first).t

module Stats : sig

  type t

  val minor_words : t -> int
  val promoted_words : t -> int
  val major_words : t -> int
  val minor_collections : t -> int
  val major_collections : t -> int
  val heap_words : t -> int
  val heap_chunks : t -> int
  val compactions : t -> int
  val top_heap_words : t -> int
  val words_scanned : t -> int
  val words_scanned_with_profinfo : t -> int

  val compare : t -> t -> int
  val hash : t -> int

end


module Snapshot : sig

  type t

  val time : t -> float
  val stats : t -> Stats.t
  val entries : t -> Entries.t
  val entries_sorted_by_words_highest_first
     : t
    -> Entries_sorted_by_words_highest_first.t

  val compare : t -> t -> int
  val hash : t -> int

  val raw : t -> Raw_spacetime_lib.Heap_snapshot.t
end

module Series : sig

  type t = Snapshot.t list

  val create : ?executable:string -> string -> t

end
