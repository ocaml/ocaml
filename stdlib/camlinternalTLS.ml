(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*      KC Sivaramakrishnan, Indian Institute of Technology, Madras       *)
(*                 Stephen Dolan, University of Cambridge                 *)
(*                   Tom Kelly, OCaml Labs Consultancy                    *)
(*                                                                        *)
(*   Copyright 2019 Indian Institute of Technology, Madras                *)
(*   Copyright 2014 University of Cambridge                               *)
(*   Copyright 2021 OCaml Labs Consultancy Ltd                            *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type dls_state = Obj.t array

let unique_value = Obj.repr (ref 0)

external get_dls_state : unit -> dls_state = "%dls_get"

external set_dls_state : dls_state -> unit =
  "caml_domain_dls_set" [@@noalloc]

type 'a key = int * (unit -> 'a)

type key_initializer =
  KI: 'a key * ('a -> 'a) -> key_initializer

let key_counter = Atomic.make 0

let parent_keys = Atomic.make ([] : key_initializer list)

let rec add_parent_key ki =
  let l = Atomic.get parent_keys in
  if not (Atomic.compare_and_set parent_keys l (ki :: l))
  then add_parent_key ki

let new_key ?split_from_parent init_orphan : _ key =
  let idx = Atomic.fetch_and_add key_counter 1 in
  let k = (idx, init_orphan) in
  begin match split_from_parent with
  | None -> ()
  | Some split -> add_parent_key (KI(k, split))
  end;
  k

(* If necessary, grow the current domain's local state array such that [idx]
 * is a valid index in the array. *)
let maybe_grow idx =
  let st = get_dls_state () in
  let sz = Array.length st in
  if idx < sz then st
  else begin
    let rec compute_new_size s =
      if idx < s then s else compute_new_size (2 * s)
    in
    let new_sz = compute_new_size (max 8 sz) in
    let new_st = Array.make new_sz unique_value in
    Array.blit st 0 new_st 0 sz;
    set_dls_state new_st;
    new_st
  end

let set (idx, _init) x =
  let st = maybe_grow idx in
  (* [Sys.opaque_identity] ensures that flambda does not look at the type of
   * [x], which may be a [float] and conclude that the [st] is a float array.
   * We do not want OCaml's float array optimisation kicking in here. *)
  st.(idx) <- Obj.repr (Sys.opaque_identity x)

let get (idx, init) =
  let st = maybe_grow idx in
  let v = st.(idx) in
  if v == unique_value then
    let v' = Obj.repr (init ()) in
    st.(idx) <- (Sys.opaque_identity v');
    Obj.magic v'
  else Obj.magic v

let get_initial_keys () : (int * Obj.t) list =
  List.map
    (fun (KI ((idx, _) as k, split)) ->
         (idx, Obj.repr (split (get k))))
    (Atomic.get parent_keys)

let set_initial_keys (l: (int * Obj.t) list) =
  List.iter
    (fun (idx, v) ->
      let st = maybe_grow idx in st.(idx) <- v)
    l
