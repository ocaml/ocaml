(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Operations on internal representations of values *)

type t

type raw_data = nativeint

external repr : 'a -> t = "%identity"
external obj : t -> 'a = "%identity"
external magic : 'a -> 'b = "%identity"
external is_int : t -> bool = "%obj_is_int"
let [@inline always] is_block a = not (is_int a)
external tag : t -> int = "caml_obj_tag" [@@noalloc]
external set_tag : t -> int -> unit = "caml_obj_set_tag"
external size : t -> int = "%obj_size"
external reachable_words : t -> int = "caml_obj_reachable_words"
external field : t -> int -> t = "%obj_field"
external set_field : t -> int -> t -> unit = "%obj_set_field"
external compare_and_swap_field : t -> int -> t -> t -> bool
  = "caml_obj_compare_and_swap"
external is_shared : t -> bool = "caml_obj_is_shared"
external floatarray_get : floatarray -> int -> float = "caml_floatarray_get"
external floatarray_set :
    floatarray -> int -> float -> unit = "caml_floatarray_set"
let [@inline always] double_field x i = floatarray_get (obj x : floatarray) i
let [@inline always] set_double_field x i v =
  floatarray_set (obj x : floatarray) i v
external raw_field : t -> int -> raw_data = "caml_obj_raw_field"
external set_raw_field : t -> int -> raw_data -> unit
                                          = "caml_obj_set_raw_field"

external new_block : int -> int -> t = "caml_obj_block"
external dup : t -> t = "caml_obj_dup"
external truncate : t -> int -> unit = "caml_obj_truncate"
external add_offset : t -> Int32.t -> t = "caml_obj_add_offset"
external with_tag : int -> t -> t = "caml_obj_with_tag"

let marshal (obj : t) =
  Marshal.to_bytes obj []
let unmarshal str pos =
  (Marshal.from_bytes str pos, pos + Marshal.total_size str pos)

let first_non_constant_constructor_tag = 0
let last_non_constant_constructor_tag = 243

let forcing_tag = 244
let cont_tag = 245
let lazy_tag = 246
let closure_tag = 247
let object_tag = 248
let infix_tag = 249
let forward_tag = 250

let no_scan_tag = 251

let abstract_tag = 251
let string_tag = 252
let double_tag = 253
let double_array_tag = 254
let custom_tag = 255
let final_tag = custom_tag


let int_tag = 1000
let out_of_heap_tag = 1001
let unaligned_tag = 1002

module Closure = struct
  type info = {
    arity: int;
    start_env: int;
  }

  let info_of_raw (info : nativeint) =
    let open Nativeint in
    let arity =
      (* signed: negative for tupled functions *)
      if Sys.word_size = 64 then
        to_int (shift_right info 56)
      else
        to_int (shift_right info 24)
    in
    let start_env =
      (* start_env is unsigned, but we know it can always fit an OCaml
         integer so we use [to_int] instead of [unsigned_to_int]. *)
      to_int (shift_right_logical (shift_left info 8) 9) in
    { arity; start_env }

  (* note: we expect a closure, not an infix pointer *)
  let info (obj : t) =
    assert (tag obj = closure_tag);
    info_of_raw (raw_field obj 1)
end

module Extension_constructor =
struct
  type t = extension_constructor
  let of_val x =
    let x = repr x in
    let slot =
      if (is_block x) && (tag x) <> object_tag && (size x) >= 1 then field x 0
      else x
    in
    let name =
      if (is_block slot) && (tag slot) = object_tag then field slot 0
      else invalid_arg "Obj.extension_constructor"
    in
      if (tag name) = string_tag then (obj slot : t)
      else invalid_arg "Obj.extension_constructor"

  let [@inline always] name (slot : t) =
    (obj (field (repr slot) 0) : string)

  let [@inline always] id (slot : t) =
    (obj (field (repr slot) 1) : int)
end

let extension_constructor = Extension_constructor.of_val
let extension_name = Extension_constructor.name
let extension_id = Extension_constructor.id

module Ephemeron = struct
  type obj_t = t

  type t (** ephemeron *)

   (** To change in sync with weak.h *)
  let additional_values = 2
  let max_ephe_length = Sys.max_array_length - additional_values

  external create : int -> t = "caml_ephe_create";;
  let create l =
    if not (0 <= l && l <= max_ephe_length) then
      invalid_arg "Obj.Ephemeron.create";
    create l

  let length x = size(repr x) - additional_values

  let raise_if_invalid_offset e o msg =
    if not (0 <= o && o < length e) then
      invalid_arg msg

  external get_key: t -> int -> obj_t option = "caml_ephe_get_key"
  let get_key e o =
    raise_if_invalid_offset e o "Obj.Ephemeron.get_key";
    get_key e o

  external get_key_copy: t -> int -> obj_t option = "caml_ephe_get_key_copy"
  let get_key_copy e o =
    raise_if_invalid_offset e o "Obj.Ephemeron.get_key_copy";
    get_key_copy e o

  external set_key: t -> int -> obj_t -> unit = "caml_ephe_set_key"
  let set_key e o x =
    raise_if_invalid_offset e o "Obj.Ephemeron.set_key";
    set_key e o x

  external unset_key: t -> int -> unit = "caml_ephe_unset_key"
  let unset_key e o =
    raise_if_invalid_offset e o "Obj.Ephemeron.unset_key";
    unset_key e o

  external check_key: t -> int -> bool = "caml_ephe_check_key"
  let check_key e o =
    raise_if_invalid_offset e o "Obj.Ephemeron.check_key";
    check_key e o

  external blit_key : t -> int -> t -> int -> int -> unit
    = "caml_ephe_blit_key"

  let blit_key e1 o1 e2 o2 l =
    if l < 0 || o1 < 0 || o1 > length e1 - l
       || o2 < 0 || o2 > length e2 - l
    then invalid_arg "Obj.Ephemeron.blit_key"
    else if l <> 0 then blit_key e1 o1 e2 o2 l

  external get_data: t -> obj_t option = "caml_ephe_get_data"
  external get_data_copy: t -> obj_t option = "caml_ephe_get_data_copy"
  external set_data: t -> obj_t -> unit = "caml_ephe_set_data"
  external unset_data: t -> unit = "caml_ephe_unset_data"
  external check_data: t -> bool = "caml_ephe_check_data"
  external blit_data : t -> t -> unit = "caml_ephe_blit_data"

end

module Effect_handlers = struct

    type _ eff = ..
    external perform : 'a eff -> 'a = "%perform"

    type ('a, 'b) stack

    external resume : ('a, 'b) stack -> ('c -> 'a) -> 'c -> 'b = "%resume"
    external runstack : ('a, 'b) stack -> ('c -> 'a) -> 'c -> 'b = "%runstack"



  module Deep = struct

    type ('a,'b) continuation
    type last_fiber

    external take_cont_noexc : ('a, 'b) continuation -> ('a, 'b) stack =
      "caml_continuation_use_noexc" [@@noalloc]
    external alloc_stack :
      ('a -> 'b) ->
      (exn -> 'b) ->
      ('c eff -> ('c, 'b) continuation -> last_fiber -> 'b) ->
      ('a, 'b) stack = "caml_alloc_stack"

    let continue k v = resume (take_cont_noexc k) (fun x -> x) v

    let discontinue k e = resume (take_cont_noexc k) (fun e -> raise e) e

    type ('a,'b) handler =
      { retc: 'a -> 'b;
        exnc: exn -> 'b;
        effc: 'c.'c eff -> (('c,'b) continuation -> 'b) option }

    external reperform :
      'a eff -> ('a, 'b) continuation -> last_fiber -> 'b = "%reperform"

    let match_with comp arg handler =
      let effc eff k last_fiber =
        match handler.effc eff with
        | Some f -> f k
        | None -> reperform eff k last_fiber
      in
      let s = alloc_stack handler.retc handler.exnc effc in
      runstack s comp arg

    type 'a effect_handler =
      { effc: 'b. 'b eff -> (('b,'a) continuation -> 'a) option }

    let try_with comp arg handler =
      let effc' eff k last_fiber =
        match handler.effc eff with
        | Some f -> f k
        | None -> reperform eff k last_fiber
      in
      let s = alloc_stack (fun x -> x) (fun e -> raise e) effc' in
      runstack s comp arg

    external drop_continuation : ('a,'b) continuation -> unit =
      "caml_drop_continuation"
  end

  module Shallow = struct

    type ('a,'b) continuation
    type last_fiber

    external alloc_stack :
      ('a -> 'b) ->
      (exn -> 'b) ->
      ('c eff -> ('c, 'b) continuation -> last_fiber -> 'b) ->
      ('a, 'b) stack = "caml_alloc_stack"


    let fiber : type a b. (a -> b) -> (a, b) continuation = fun f ->
      let module M = struct type _ eff += Initial_setup__ : a eff end in
      let exception E of (a,b) continuation in
      let f' () = f (perform M.Initial_setup__) in
      let error _ = failwith "impossible" in
      let effc eff k _last_fiber =
        match eff with
        | M.Initial_setup__ -> raise (E k)
        | _ -> error ()
      in
      let s = alloc_stack error error effc in
      try magic (runstack s f' ()) with E k -> k

    type ('a,'b) handler =
      { retc: 'a -> 'b;
        exnc: exn -> 'b;
        effc: 'c.'c eff -> (('c,'a) continuation -> 'b) option }

    external update_handler :
      ('a,'b) continuation ->
      ('b -> 'c) ->
      (exn -> 'c) ->
      ('d eff -> ('d,'b) continuation -> last_fiber -> 'c) ->
      ('a,'c) stack = "caml_continuation_use_and_update_handler_noexc" [@@noalloc]

    external reperform :
      'a eff -> ('a, 'b) continuation -> last_fiber -> 'c = "%reperform"

    let continue_with k v handler =
      let effc eff k last_fiber =
        match handler.effc eff with
        | Some f -> f k
        | None -> reperform eff k last_fiber
      in
      let stack = update_handler k handler.retc handler.exnc effc in
      resume stack (fun x -> x) v

    let discontinue_with k x handler =
      let effc eff k last_fiber =
        match handler.effc eff with
        | Some f -> f k
        | None -> reperform eff k last_fiber
      in
      let stack = update_handler k handler.retc handler.exnc effc in
      resume stack (fun e -> raise e) x

    external drop_continuation : ('a,'b) continuation -> unit =
      "caml_drop_continuation"
  end
end
