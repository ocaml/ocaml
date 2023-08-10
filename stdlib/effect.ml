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

type 'a name = ..

module type Name = sig
  type t
  type 'a name += N : t name
  val name : string
end

type 'a t = (module Name with type t = 'a)

let default_name = "<unnamed>"

let create (type e) ?(name=default_name) () : e t =
  (module struct
     type t = e
     type 'a name += N : t name
     let name = name
   end)

let equal (type a b) ((module A) : a t) ((module B) : b t)
    : (a, b) Type.eq option =
  match A.N with
  | B.N -> Some Type.Equal
  | _ -> None

let name (type a) ((module N) : a t) = N.name

external raw_perform : 'e t * ('a, 'e) operation -> 'a = "%perform"

let perform e op =
  let res = raw_perform (e, op) in
  res

type ('a, 'b) stack [@@immediate]
type last_fiber [@@immediate]

let is_null_stack (stk : (_, _) stack) =
  Int.equal 0 (Obj.magic stk)

external resume :
  ('a, 'b) stack -> ('c -> 'a) -> 'c -> last_fiber -> 'b = "%resume"

type ('a,'b) continuation

external take_cont_noexc : ('a, 'b) continuation -> ('a, 'b) stack =
  "caml_continuation_use_noexc" [@@noalloc]
external cont_last_fiber : ('a, 'b) continuation -> last_fiber = "%field1"
external cont_set_last_fiber :
  ('a, 'b) continuation -> last_fiber -> unit = "%setfield1"
external cont_set_stack :
  ('a, 'b) continuation -> ('a, 'b) stack -> unit = "%setfield0"

let continue k v =
  resume (take_cont_noexc k) (fun x -> x) v (cont_last_fiber k)

let discontinue k e =
  resume (take_cont_noexc k) (fun e -> raise e) e (cont_last_fiber k)

let discontinue_with_backtrace k e bt =
  resume (take_cont_noexc k) (fun e -> Printexc.raise_with_backtrace e bt)
    e (cont_last_fiber k)

external get_callstack :
  ('a,'b) continuation -> int -> Printexc.raw_backtrace =
  "caml_get_continuation_callstack"

external runstack : ('a, 'b) stack -> ('c -> 'a) -> 'c -> 'b = "%runstack"

external alloc_stack :
  ('a -> 'b) ->
  (exn -> 'b) ->
  (('e t * ('k, 'e) operation) -> ('k, 'b) continuation -> last_fiber -> 'b) ->
  ('a, 'b) stack = "caml_alloc_stack"

external raw_reperform :
  ('e t * ('a, 'e) operation)
  -> ('a, 'b) continuation
  -> last_fiber
  -> 'b = "%reperform"

type ('a, 'e) step =
  | Result of 'a
  | Exn of exn
  | Operation :
      ('o, 'e) operation * ('o, ('a, 'e) step) continuation -> ('a, 'e) step
(** [('a, 'e) step] is the result of running an effect handler until it
    returns a result, raises an exception or performs an operation. *)

let run (type e) (e : e t) comp arg =
  let result v = Result v in
  let exn e = Exn e in
  let operation (type f) (((f : f t), op) as eff) k last_fiber =
    match equal e f with
    | Some Type.Equal ->
        cont_set_last_fiber k last_fiber;
        Operation(op, k)
    | None -> raw_reperform eff k last_fiber
  in
  let s = alloc_stack result exn operation in
  runstack s comp arg

type 'e initial = effect
  | Initial : 'a

let fiber (type a b e) (e : e t) (f : a -> b) =
  let initial : a initial t = create ~name:"<initial>" () in
  let exception E of (a, (b, e) step) continuation in
  let comp () = f (perform initial Initial) in
  let result v = Result v in
  let exn e = Exn e in
  let operation
        (type f) (((f : f t), (op : (_, f) operation)) as eff)
        (k : (a, (b, e) step) continuation) last_fiber
        : (b, e) step =
    match equal e f with
    | Some Type.Equal ->
        cont_set_last_fiber k last_fiber;
        Operation(op, k)
    | None ->
        match equal initial f with
        | Some Type.Equal ->
            cont_set_last_fiber k last_fiber;
            raise_notrace (E k)
        | None ->
            raw_reperform eff k last_fiber
  in
  let s = alloc_stack result exn operation in
  match runstack s comp () with
  | exception E k -> k
  | _ -> failwith "impossible"

type ('a, 'e, 'b) handler =
  { result: 'a -> 'b;
    exn: exn -> 'b;
    operation: 'k. ('k, 'e) operation -> ('k, 'b) continuation -> 'b }

let run_with (type e) (e : e t) comp arg (handler : (_, e, _) handler) =
  let operation
    (type f) (((f : f t), (op : (_, f) operation)) as eff) k last_fiber =
    match equal e f with
    | Some Type.Equal ->
        cont_set_last_fiber k last_fiber;
        handler.operation op k
    | None -> raw_reperform eff k last_fiber
  in
  let s = alloc_stack handler.result handler.exn operation in
  runstack s comp arg


exception Unhandled : 'e t * ('a, 'e) operation -> exn
exception Continuation_already_resumed

let reperform t op k =
  let k' : _ continuation = Obj.obj (Obj.new_block Obj.cont_tag 2) in
  let stack = take_cont_noexc k in
  cont_set_stack k' stack;
  let last_fiber = cont_last_fiber k in
  cont_set_last_fiber k' last_fiber;
  if is_null_stack stack then raise Continuation_already_resumed
  else raw_reperform (t, op) k' last_fiber

type 'a eff = 'a t

module Legacy = struct

  exception Eff_unhandled = Unhandled

  type _ t = ..

  type nonrec legacy = effect
    | Legacy : 'a t -> 'a

  let legacy : legacy eff = create ~name:"legacy" ()

  exception Unhandled : 'a t -> exn

  exception Continuation_already_resumed = Continuation_already_resumed

  module Deep = struct

    type nonrec ('a,'b) continuation =
      ('a, 'b) continuation

    let continue = continue

    let discontinue = discontinue

    let discontinue_with_backtrace = discontinue_with_backtrace

    type ('a,'b) handler =
      { retc: 'a -> 'b;
        exnc: exn -> 'b;
        effc: 'c.'c t -> (('c,'b) continuation -> 'b) option }

    let match_with comp arg handler =
      let result = handler.retc in
      let exn = handler.exnc in
      let operation ((Legacy t) as op) k =
        match handler.effc t with
        | None -> raw_reperform (legacy, op) k (cont_last_fiber k)
        | Some f -> f k
      in
      let handler = { result; exn; operation } in
      run_with legacy comp arg handler

    type 'a effect_handler =
      { effc: 'b. 'b t -> (('b,'a) continuation -> 'a) option }

    let try_with comp arg handler =
      let result x = x in
      let exn e = raise e in
      let operation ((Legacy t) as op) k =
        match handler.effc t with
        | None -> raw_reperform (legacy, op) k (cont_last_fiber k)
        | Some f -> f k
      in
      let handler = { result; exn; operation } in        
      run_with legacy comp arg handler

    let get_callstack = get_callstack

  end

  module Shallow = struct

    type nonrec ('a,'b) continuation =
      ('a, ('b, legacy) step) continuation

    let fiber (type a b) (f : a -> b) : (a, b) continuation =
      fiber legacy f

    type ('a,'b) handler =
      { retc: 'a -> 'b;
        exnc: exn -> 'b;
        effc: 'c.'c t -> (('c,'a) continuation -> 'b) option }

    let continue_gen
          (type a b)
          cont k v (handler : (a, b) handler) =
      let rec loop step =
        match step with
        | Result v -> handler.retc v
        | Exn e -> handler.exnc e
        | Operation(Legacy t as op, k) ->
            match handler.effc t with
            | None ->
                let step =
                  (fun () ->
                    raw_reperform (legacy, op) k (cont_last_fiber k)) ()
                in
                loop step
            | Some f ->
                f k
      in
      loop (cont k v)

    let continue_with k v handler =
      continue_gen continue k v handler

    let discontinue_with k e handler =
      continue_gen discontinue k e handler

    let discontinue_with_backtrace k v bt handler =
      continue_gen
        (fun k e -> discontinue_with_backtrace k e bt)
        k v handler

    let get_callstack = get_callstack

  end

  let perform t =
    try
      raw_perform (legacy, Legacy t)
    with Eff_unhandled(eff, op) as exn ->
      match equal legacy eff with
      | None -> raise exn
      | Some Type.Equal ->
          let Legacy t = op in
          raise (Unhandled t)

end

let () =
  let printer = function
    | Unhandled(t, _) ->
        let msg = Printf.sprintf "Stdlib.Effect.Unhandled(%s, _)" (name t) in
        Some msg
    | Legacy.Unhandled t ->
        let msg =
          Printf.sprintf "Stdlib.Effect.Legacy.Unhandled(%s)"
            (Printexc.string_of_extension_constructor @@ Obj.repr t)
        in
        Some msg
    | _ -> None
  in
  Printexc.register_printer printer

(* Register the exceptions so that the runtime can access it *)
type dummy = effect Dummy : unit
let dummy = create ~name:"<dummy>" ()
let _ = Callback.register_exception "Effect.Unhandled"
          (Unhandled(dummy, Dummy))
let _ = Callback.register_exception "Effect.Continuation_already_resumed"
          Continuation_already_resumed
