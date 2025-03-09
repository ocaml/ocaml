(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                     Anmol Sahoo, Purdue University                     *)
(*                        Olivier Nicole, Tarides                         *)
(*                         Fabrice Buoro, Tarides                         *)
(*                                                                        *)
(*   Copyright 2023 Tarides                                               *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Instrumentation of memory accesses to use ThreadSanitizer (TSan) for data
    race detection. This module contains an instrumentation pass on Cmm, where
    most of the instrumentation happens.

    TSan requires to instrument all memory accesses (to mutable data), thread
    spawning and joining, mutex operations, and all such events that are
    relevant for parallelism. Thread and mutex operations are instrumented by
    the C compiler via the runtime. Instrumentation calls are FFI (C) calls.

    TSan also requires to instrument the entry and exit of each function. TSan
    records function entries and exits along with other events in a history in
    to be able to print backtraces of an event (memory access, mutex
    creation...) when needed.

    In addition to this static instrumentation, we must let TSan know when a
    function is exited due to an exception or when performing effect, or
    re-entered when resuming a continuation. This dynamic instrumentation is
    performed by dedicated runtime functions in runtime/tsan.c. These functions
    are called from the assembly chunks of the runtime.
 *)

(** Instrumentation of a {!Cmm.expression}: instrument memory accesses, and
    surround the expression by external calls to [__tsan_func_entry] and
    [__tsan_func_exit]. If the expression tail is a function call, then
    [__tsan_func_exit] is inserted before that call. *)
val instrument : Cmm.expression -> Cmm.expression

(** Surround an expression by external calls to [__tsan_func_entry] and
    [__tsan_func_exit]. If the expression tail is a function call, then
    [__tsan_func_exit] is inserted before that call. *)
val wrap_entry_exit : Cmm.expression -> Cmm.expression

(** Call to [__tsan_init], which should be called at least once in the compiled
    program, before other [__tsan_*] API functions. [__tsan_init] is
    idempotent, i.e. can be called more than once without consequences. *)
val init_code : unit -> Cmm.expression
