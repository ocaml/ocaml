(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                        Frederic Bour, Tarides                          *)
(*                         Thomas Refis, Tarides                          *)
(*                                                                        *)
(*   Copyright 2020 Tarides                                               *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type ref_and_reset =
  | Table : { ref: 'a ref; init: unit -> 'a } -> ref_and_reset
  | Immutable : { ref: 'a ref; mutable snapshot: 'a } -> ref_and_reset

type bindings = {
  mutable refs: ref_and_reset list;
  mutable frozen : bool;
  is_bound: bool ref
}

let global_bindings =
  { refs = []; is_bound = ref false; frozen = false }

let is_bound () = !(global_bindings.is_bound)

let reset () =
  assert (is_bound ());
  List.iter (function
    | Table { ref; init } -> ref := init ()
    | Immutable { ref; snapshot } -> ref := snapshot
  ) global_bindings.refs

let s_table create size =
  let init () = create size in
  let ref = ref (init ()) in
  assert (not global_bindings.frozen);
  global_bindings.refs <- (Table { ref; init }) :: global_bindings.refs;
  ref

let s_ref k =
  let ref = ref k in
  assert (not global_bindings.frozen);
  global_bindings.refs <-
    (Immutable { ref; snapshot = k }) :: global_bindings.refs;
  ref

type slot = Slot : { ref : 'a ref; mutable value : 'a } -> slot
type scope = { slots: slot list; scope_bound : bool ref }

let fresh () =
  let slots =
    List.map (function
      | Table { ref; init } -> Slot {ref; value = init ()}
      | Immutable r ->
          if not global_bindings.frozen then r.snapshot <- !(r.ref);
          Slot { ref = r.ref; value = r.snapshot }
    ) global_bindings.refs
  in
  global_bindings.frozen <- true;
  { slots; scope_bound = global_bindings.is_bound }

let with_scope { slots; scope_bound } f =
  assert (not !scope_bound);
  scope_bound := true;
  List.iter (fun (Slot {ref;value}) -> ref := value) slots;
  Fun.protect f ~finally:(fun () ->
    List.iter (fun (Slot s) -> s.value <- !(s.ref)) slots;
    scope_bound := false
  )
