/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 2001 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#ifndef CAML_BACKTRACE_H
#define CAML_BACKTRACE_H

#ifdef CAML_INTERNALS

#include "mlvalues.h"
#include "exec.h"

/* Runtime support for backtrace generation.
 *
 * It has two kind of users:
 * - high-level API to capture and decode backtraces;
 * - low-level runtime routines, to introspect machine state and determine
 *   whether a backtrace should be generated when using "raise".
 *
 * Backtrace generation is split in multiple steps.
 * The lowest-level one, done by [backtrace_byt.c] and
 * [backtrace_nat.c] just fills the [Caml_state->backtrace_buffer]
 * variable each time a frame is unwinded.
 * At that point, we don't know whether the backtrace will be useful or not so
 * this code should be as fast as possible.
 *
 * If the backtrace happens to be useful, later passes will read
 * [Caml_state->backtrace_buffer] and turn it into a [raw_backtrace] and then a
 * [backtrace].
 * This is done in [backtrace.c] and [stdlib/printexc.ml].
 *
 * Content of buffers
 * ------------------
 *
 * [Caml_state->backtrace_buffer] (really cheap)
 *   Backend and process image dependent, abstracted by C-type backtrace_slot.
 * [raw_backtrace] (cheap)
 *   OCaml values of abstract type [Printexc.raw_backtrace_slot],
 *   still backend and process image dependent (unsafe to marshal).
 * [backtrace] (more expensive)
 *   OCaml values of algebraic data-type [Printexc.backtrace_slot]
 */
 /* [Caml_state->backtrace_active] is non zero iff backtraces are recorded.
 * This variable must be changed with [caml_record_backtrace].
 */
#define caml_backtrace_active (Caml_state_field(backtrace_active))
/* The [Caml_state->backtrace_buffer] and [Caml_state->backtrace_last_exn]
 * variables are valid only if [Caml_state->backtrace_active != 0].
 *
 * They are part of the state specific to each thread, and threading libraries
 * are responsible for copying them on context switch.
 * See [otherlibs/systhreads/st_stubs.c].
 *
 *
 * [Caml_state->backtrace_buffer] is filled by runtime when unwinding stack. It
 * is an array ranging from [0] to [Caml_state->backtrace_pos - 1].
 * [Caml_state->backtrace_pos] is always zero if
 * [!Caml_state->backtrace_active].
 *
 * Its maximum size is determined by [BACKTRACE_BUFFER_SIZE] from
 * [backtrace_prim.h], but this shouldn't affect users.
 */
#define caml_backtrace_buffer (Caml_state_field(backtrace_buffer))
#define caml_backtrace_pos (Caml_state_field(backtrace_pos))

/* [Caml_state->backtrace_last_exn] stores the last exception value that was
 * raised, iff [Caml_state->backtrace_active != 0]. It is tested for equality
 * to determine whether a raise is a re-raise of the same exception.
 */
#define caml_backtrace_last_exn (Caml_state_field(backtrace_last_exn))

/* FIXME: this shouldn't matter anymore. Since OCaml 4.02, non-parameterized
 * exceptions are constant, so physical equality is no longer appropriate.
 * raise and re-raise are distinguished by:
 * - passing reraise = 1 to [caml_stash_backtrace] (see below) in the bytecode
 *   interpreter;
 * - directly resetting [Caml_state->backtrace_pos] to 0 in native
     runtimes for raise.
 */

/* [caml_record_backtrace] toggle backtrace recording on and off.
 * This function can be called at runtime by user-code, or during
 * initialization if backtraces were requested.
 *
 * It might be called before GC initialization, so it shouldn't do OCaml
 * allocation.
 */
CAMLextern value caml_record_backtrace(value vflag);


#ifndef NATIVE_CODE

/* Path to the file containing debug information, if any, or NULL. */
CAMLextern char_os * caml_cds_file;

/* Primitive called _only_ by runtime to record unwinded frames to
 * backtrace.  A similar primitive exists for native code, but with a
 * different prototype. */
extern void caml_stash_backtrace(value exn, value * sp, int reraise);

CAMLextern void caml_load_main_debug_info(void);
#endif


/* Default (C-level) printer for backtraces.  It is called if an
 * exception causes a termination of the program or of a thread.
 *
 * [Printexc] provide a higher-level printer mimicking its output but making
 * use of registered exception printers, and is used when possible in place of
 * this function after [Printexc] initialization.
 */
CAMLextern void caml_print_exception_backtrace(void);

void caml_init_backtrace(void);
CAMLextern void caml_init_debug_info(void);

#endif /* CAML_INTERNALS */

#endif /* CAML_BACKTRACE_H */
