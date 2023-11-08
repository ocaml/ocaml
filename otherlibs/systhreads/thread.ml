(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*   Xavier Leroy and Pascal Cuoq, projet Cristal, INRIA Rocquencourt     *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* User-level threads *)

type t

external thread_initialize : unit -> unit = "caml_thread_initialize"
external thread_cleanup : unit -> unit = "caml_thread_cleanup"
external thread_new : (unit -> unit) -> t = "caml_thread_new"
external thread_uncaught_exception : exn -> unit =
            "caml_thread_uncaught_exception"

external yield : unit -> unit = "caml_thread_yield"
external self : unit -> t = "caml_thread_self" [@@noalloc]
external id : t -> int = "caml_thread_id" [@@noalloc]
external join : t -> unit = "caml_thread_join"

(* For new, make sure the function passed to thread_new never
   raises an exception. *)

let[@inline never] check_memprof_cb () = ref ()

let default_uncaught_exception_handler = thread_uncaught_exception

let uncaught_exception_handler = ref default_uncaught_exception_handler

let set_uncaught_exception_handler fn = uncaught_exception_handler := fn

exception Exit

let create fn arg =
  thread_new
    (fun () ->
      try
        fn arg;
        ignore (Sys.opaque_identity (check_memprof_cb ()))
      with
      | Exit ->
        ignore (Sys.opaque_identity (check_memprof_cb ()))
      | exn ->
        let raw_backtrace = Printexc.get_raw_backtrace () in
        flush stdout; flush stderr;
        try
          !uncaught_exception_handler exn
        with
        | Exit -> ()
        | exn' ->
          Printf.eprintf
            "Thread %d killed on uncaught exception %s\n"
            (id (self ())) (Printexc.to_string exn);
          Printexc.print_raw_backtrace stderr raw_backtrace;
          Printf.eprintf
            "Thread %d uncaught exception handler raised %s\n"
            (id (self ())) (Printexc.to_string exn');
          Printexc.print_backtrace stdout;
          flush stderr)

let exit () =
  raise Exit

(* Initialization of the scheduler *)

let () =
  thread_initialize ();
  (* Called back in [caml_shutdown], when the last domain exits. *)
  Callback.register "Thread.at_shutdown" thread_cleanup

(* Wait functions *)

let delay = Unix.sleepf

let wait_timed_read fd d =
  match Unix.select [fd] [] [] d with ([], _, _) -> false | (_, _, _) -> true
let wait_timed_write fd d =
  match Unix.select [] [fd] [] d with (_, [], _) -> false | (_, _, _) -> true
let select = Unix.select

let wait_pid p = Unix.waitpid [] p

external sigmask : Unix.sigprocmask_command -> int list -> int list
   = "caml_thread_sigmask"
external wait_signal : int list -> int = "caml_wait_signal"


module TLS = struct
  type 'a key = {
    index: int;  (** Unique index for this key. *)
    compute: unit -> 'a;
        (** Initializer for values for this key. Called at most
          once per thread. *)
  }

  (** Counter used to allocate new keys *)
  let counter = Atomic.make 0

  (** Value used to detect a TLS slot that was not initialized yet.
      Because [counter] is private and lives forever, no other
      object the user can see will have the same address. *)
  let[@inline] sentinel_value_for_uninit_tls_ () : Obj.t = Obj.repr counter

  let new_key compute : _ key =
    let index = Atomic.fetch_and_add counter 1 in
    { index; compute }

  let ceil_pow_2_minus_1 (n : int) : int =
    let n = n lor (n lsr 1) in
    let n = n lor (n lsr 2) in
    let n = n lor (n lsr 4) in
    let n = n lor (n lsr 8) in
    let n = n lor (n lsr 16) in
    if Sys.int_size > 32 then
      n lor (n lsr 32)
    else
      n

  (** Grow the array so that [index] is valid. *)
  let[@inline never] grow_tls (old : Obj.t array) (index : int) : Obj.t array =
    let new_length = ceil_pow_2_minus_1 (index + 1) in
    let new_ = Array.make new_length (sentinel_value_for_uninit_tls_ ()) in
    Array.blit old 0 new_ 0 (Array.length old);
    new_

  external get_tls_root : unit -> Obj.t = "caml_thread_tls_get" [@@noalloc]

  external set_tls_root : Obj.t -> unit = "caml_thread_tls_set" [@@noalloc]

  let[@inline] get_tls_ (index : int) : Obj.t array =
    let tls = get_tls_root () in
    if tls == Obj.repr () then (
      let new_tls = grow_tls [||] index in
      set_tls_root (Obj.repr new_tls);
      new_tls
    ) else (
      let tls = (Obj.obj tls : Obj.t array) in
      if index < Array.length tls then
        tls
      else (
        let new_tls = grow_tls tls index in
        set_tls_root (Obj.repr new_tls);
        new_tls
      )
    )

  let[@inline never] get_compute_ tls (key : 'a key) : 'a =
    let value = key.compute () in
    Array.unsafe_set tls key.index (Obj.repr (Sys.opaque_identity value));
    value

  let[@inline] get (key : 'a key) : 'a =
    let tls = get_tls_ key.index in
    let value = Array.unsafe_get tls key.index in
    if value != sentinel_value_for_uninit_tls_ () then
      (* fast path *)
      Obj.obj value
    else
      (* slow path: we need to compute the initial value *)
      get_compute_ tls key

  let[@inline] set key value : unit =
    let tls = get_tls_ key.index in
    Array.unsafe_set tls key.index (Obj.repr (Sys.opaque_identity value))
end
