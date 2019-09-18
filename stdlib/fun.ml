(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                         The OCaml programmers                          *)
(*                                                                        *)
(*   Copyright 2018 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

external id : 'a -> 'a = "%identity"
let const c _ = c
let flip f x y = f y x
let negate p v = not (p v)

exception Finally_raised of exn

let () = Printexc.register_printer @@ function
| Finally_raised exn -> Some ("Fun.Finally_raised: " ^ Printexc.to_string exn)
| _ -> None

let protect ~(finally : unit -> unit) work =
  let finally_no_exn () =
    try finally () with e ->
      let bt = Printexc.get_raw_backtrace () in
      Printexc.raise_with_backtrace (Finally_raised e) bt
  in
  match work () with
  | result -> finally_no_exn () ; result
  | exception work_exn ->
      let work_bt = Printexc.get_raw_backtrace () in
      finally_no_exn () ;
      Printexc.raise_with_backtrace work_exn work_bt

external uncaught_exception_in_destructor : exn -> 'a
  = "caml_uncaught_exception_in_destructor"

type _mask_kind = Mask_none | Mask_uninterruptible | Mask_nonpreemptible

external set_mask : _mask_kind -> _mask_kind = "caml_sys_set_mask" [@@noalloc]
external unset_mask : _mask_kind -> unit = "caml_sys_unset_mask" [@@noalloc]

let with_resource ~acquire x ~scope ~(release : _ -> unit) =
  (* we inline Sys.mask to avoid allocations *)
  let release_no_exn (* BEGIN ATOMIC *) ~release resource =
    let old_mask = set_mask Mask_uninterruptible in
    (* END ATOMIC *)
    match release resource with
    | () -> unset_mask old_mask
    | exception e -> (
        (* BEGIN ATOMIC *)
        unset_mask old_mask ;
        uncaught_exception_in_destructor e
        (* END ATOMIC *)
      )
  in
  let old_mask = set_mask Mask_uninterruptible in
  (* asynchronous exceptions are turned off *)
  let resource =
    try acquire x
    with e -> (
        (* BEGIN ATOMIC *)
        unset_mask old_mask ;
        raise e
        (* END ATOMIC *)
      )
  in
  match
    unset_mask old_mask ;
    scope resource
  with
  | (* BEGIN ATOMIC *) result -> (
      release_no_exn ~release resource ;
      (* END ATOMIC *)
      result
    )
  | (* BEGIN ATOMIC *) exception e -> (
      let work_bt = Printexc.get_raw_backtrace () in
      release_no_exn ~release resource ;
      Printexc.raise_with_backtrace e work_bt
      (* END ATOMIC *)
    )
