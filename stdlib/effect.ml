(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*      KC Sivaramakrishnan, Indian Institute of Technology, Madras       *)
(*                                                                        *)
(*   Copyright 2021 Indian Institute of Technology, Madras                *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

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

  let discontinue_with_backtrace k e bt = resume (take_cont_noexc k) (fun e ->
    Printexc.raise_with_backtrace e bt) e

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

  external get_callstack :
    ('a,'b) continuation -> int -> Printexc.raw_backtrace =
    "caml_get_continuation_callstack"
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
    try Obj.magic (runstack s f' ()) with E k -> k

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

  let discontinue_with_backtrace k x bt handler =
    let effc eff k last_fiber =
      match handler.effc eff with
      | Some f -> f k
      | None -> reperform eff k last_fiber
    in
    let stack = update_handler k handler.retc handler.exnc effc in
    resume stack (fun e -> Printexc.raise_with_backtrace e bt) x

  external get_callstack :
    ('a,'b) continuation -> int -> Printexc.raw_backtrace =
    "caml_get_continuation_callstack"
end
