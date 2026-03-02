(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Sacha-Élie Ayoun, Soteria Tools Ltd.                  *)
(*                                                                        *)
(*   Copyright 2026, Soteria Tools Ltd.                                   *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type 'a t = ('a, unit) Hashtbl.t

let create = Hashtbl.create
let clear = Hashtbl.clear
let reset = Hashtbl.reset
let copy = Hashtbl.copy
let[@inline] add h x = Hashtbl.replace h x ()
let remove = Hashtbl.remove
let mem = Hashtbl.mem
let[@inline] iter f h = Hashtbl.iter (fun x () -> f x) h

let[@inline] filter_inplace f h =
  Hashtbl.filter_map_inplace (fun x () -> if f x then Some () else None) h

let[@inline] fold f h acc = Hashtbl.fold (fun x () acc -> f x acc) h acc
let length = Hashtbl.length
let randomize = Hashtbl.randomize
let is_randomized = Hashtbl.is_randomized
let rebuild = Hashtbl.rebuild
let stats = Hashtbl.stats
let to_seq = Hashtbl.to_seq_keys
let add_seq h seq = Seq.iter (fun x -> add h x) seq

let of_seq seq =
  let h = create 16 in
  add_seq h seq;
  h

module type S = sig
  type elt
  type t

  val create : int -> t
  val clear : t -> unit
  val reset : t -> unit
  val copy : t -> t
  val add : t -> elt -> unit
  val remove : t -> elt -> unit
  val mem : t -> elt -> bool
  val iter : (elt -> unit) -> t -> unit
  val filter_inplace : (elt -> bool) -> t -> unit
  val fold : (elt -> 'acc -> 'acc) -> t -> 'acc -> 'acc
  val length : t -> int
  val stats : t -> Hashtbl.statistics
  val to_seq : t -> elt Seq.t
  val add_seq : t -> elt Seq.t -> unit
  val of_seq : elt Seq.t -> t
end

module type SeededS = sig
  (** The type of elements of the hash set. *)
  type elt

  (** The type of hash sets. *)
  type t

  val create : ?random:(* thwart tools/sync_stdlib_docs *) bool -> int -> t
  val clear : t -> unit
  val reset : t -> unit
  val copy : t -> t
  val add : t -> elt -> unit
  val remove : t -> elt -> unit
  val mem : t -> elt -> bool
  val iter : (elt -> unit) -> t -> unit
  val filter_inplace : (elt -> bool) -> t -> unit
  val fold : (elt -> 'acc -> 'acc) -> t -> 'acc -> 'acc
  val length : t -> int
  val stats : t -> Hashtbl.statistics
  val to_seq : t -> elt Seq.t
  val add_seq : t -> elt Seq.t -> unit
  val of_seq : elt Seq.t -> t
end

module MakeSeeded (H : Hashtbl.SeededHashedType) : SeededS with type elt = H.t =
struct
  module HASHTBL = Hashtbl.MakeSeeded (H)

  type elt = H.t
  type nonrec t = unit HASHTBL.t

  let create = HASHTBL.create
  let clear = HASHTBL.clear
  let reset = HASHTBL.reset
  let copy = HASHTBL.copy
  let add h elt = HASHTBL.replace h elt ()
  let remove = HASHTBL.remove
  let mem = HASHTBL.mem
  let iter f h = HASHTBL.iter (fun elt () -> f elt) h

  let filter_inplace f h =
    HASHTBL.filter_map_inplace (fun elt () -> if f elt then Some () else None) h

  let fold f h acc = HASHTBL.fold (fun elt () acc -> f elt acc) h acc
  let length = HASHTBL.length
  let stats = HASHTBL.stats
  let to_seq = HASHTBL.to_seq_keys
  let add_seq h seq = Seq.iter (fun elt -> add h elt) seq

  let of_seq seq =
    let h = create 16 in
    add_seq h seq;
    h
end

module Make (H : Hashtbl.HashedType) : S with type elt = H.t = struct
  include MakeSeeded (struct
    type t = H.t

    let equal = H.equal
    let seeded_hash (_seed : int) x = H.hash x
  end)

  let create sz = create ~random:false sz

  let of_seq i =
    let tbl = create 16 in
    add_seq tbl i;
    tbl
end
