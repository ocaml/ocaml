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

type 'a t = ..
external perform : 'a t -> 'a = "%perform"

type exn += Unhandled: 'a t -> exn
exception Continuation_already_resumed

let () =
  let printer = function
    | Unhandled x ->
        let msg = Printf.sprintf "Stdlib.Effect.Unhandled(%s)"
            (Printexc.string_of_extension_constructor @@ Obj.repr x)
        in
        Some msg
    | _ -> None
  in
  Printexc.register_printer printer

(* Register the exceptions so that the runtime can access it *)
type _ t += Should_not_see_this__ : unit t
let _ = Callback.register_exception "Effect.Unhandled"
          (Unhandled Should_not_see_this__)
let _ = Callback.register_exception "Effect.Continuation_already_resumed"
          Continuation_already_resumed

type ('a, 'b) stack [@@immediate]
type last_fiber [@@immediate]

external resume :
  ('a, 'b) stack -> ('c -> 'a) -> 'c -> last_fiber -> 'b = "%resume"
external runstack : ('a, 'b) stack -> ('c -> 'a) -> 'c -> 'b = "%runstack"

module Deep = struct

  type ('a,'b) continuation

  external take_cont_noexc : ('a, 'b) continuation -> ('a, 'b) stack =
    "caml_continuation_use_noexc" [@@noalloc]
  external alloc_stack :
    ('a -> 'b) ->
    (exn -> 'b) ->
    ('c t -> ('c, 'b) continuation -> last_fiber -> 'b) ->
    ('a, 'b) stack = "caml_alloc_stack"
  external cont_last_fiber : ('a, 'b) continuation -> last_fiber = "%field1"
  external cont_set_last_fiber :
    ('a, 'b) continuation -> last_fiber -> unit = "%setfield1"

  let continue k v =
    resume (take_cont_noexc k) (fun x -> x) v (cont_last_fiber k)

  let discontinue k e =
    resume (take_cont_noexc k) (fun e -> raise e) e (cont_last_fiber k)

  let discontinue_with_backtrace k e bt =
    resume (take_cont_noexc k) (fun e -> Printexc.raise_with_backtrace e bt)
      e (cont_last_fiber k)

  type ('a,'b) handler =
    { retc: 'a -> 'b;
      exnc: exn -> 'b;
      effc: 'c.'c t -> (('c,'b) continuation -> 'b) option }

  external reperform :
    'a t -> ('a, 'b) continuation -> last_fiber -> 'b = "%reperform"

  let match_with comp arg handler =
    let effc eff k last_fiber =
      match handler.effc eff with
      | Some f ->
          cont_set_last_fiber k last_fiber;
          f k
      | None -> reperform eff k last_fiber
    in
    let s = alloc_stack handler.retc handler.exnc effc in
    runstack s comp arg

  type 'a effect_handler =
    { effc: 'b. 'b t -> (('b,'a) continuation -> 'a) option }

  let try_with comp arg handler =
    let effc' eff k last_fiber =
      match handler.effc eff with
      | Some f ->
          cont_set_last_fiber k last_fiber;
          f k
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

  external alloc_stack :
    ('a -> 'b) ->
    (exn -> 'b) ->
    ('c t -> ('c, 'b) continuation -> last_fiber -> 'b) ->
    ('a, 'b) stack = "caml_alloc_stack"

  external cont_last_fiber : ('a, 'b) continuation -> last_fiber = "%field1"
  external cont_set_last_fiber :
    ('a, 'b) continuation -> last_fiber -> unit = "%setfield1"

  let fiber : type a b. (a -> b) -> (a, b) continuation = fun f ->
    let module M = struct type _ t += Initial_setup__ : a t end in
    let exception E of (a,b) continuation in
    let f' () = f (perform M.Initial_setup__) in
    let error _ = failwith "impossible" in
    let effc eff k last_fiber =
      match eff with
      | M.Initial_setup__ ->
          cont_set_last_fiber k last_fiber;
          raise_notrace (E k)
      | _ -> error ()
    in
    let s = alloc_stack error error effc in
    match runstack s f' () with
    | exception E k -> k
    | _ -> error ()

  type ('a,'b) handler =
    { retc: 'a -> 'b;
      exnc: exn -> 'b;
      effc: 'c.'c t -> (('c,'a) continuation -> 'b) option }

  external update_handler :
    ('a,'b) continuation ->
    ('b -> 'c) ->
    (exn -> 'c) ->
    ('d t -> ('d,'b) continuation -> last_fiber -> 'c) ->
    ('a,'c) stack = "caml_continuation_use_and_update_handler_noexc" [@@noalloc]

  external reperform :
    'a t -> ('a, 'b) continuation -> last_fiber -> 'c = "%reperform"

  let continue_gen k resume_fun v handler =
    let effc eff k last_fiber =
      match handler.effc eff with
      | Some f ->
          cont_set_last_fiber k last_fiber;
          f k
      | None -> reperform eff k last_fiber
    in
    let last_fiber = cont_last_fiber k in
    let stack = update_handler k handler.retc handler.exnc effc in
    resume stack resume_fun v last_fiber

  let continue_with k v handler =
    continue_gen k (fun x -> x) v handler

  let discontinue_with k v handler =
    continue_gen k (fun e -> raise e) v handler

  let discontinue_with_backtrace k v bt handler =
    continue_gen k (fun e -> Printexc.raise_with_backtrace e bt) v handler

  external get_callstack :
    ('a,'b) continuation -> int -> Printexc.raw_backtrace =
    "caml_get_continuation_callstack"
end
