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

#ifndef CAML_BACKTRACE_PRIM_H
#define CAML_BACKTRACE_PRIM_H

#ifdef CAML_INTERNALS

#include "backtrace.h"

/* Backtrace generation is split in [backtrace.c] and [backtrace_prim].
 *
 * [backtrace_prim] contains all backend-specific
 * code, and has two different
 * implementations in [runtime/backtrace_byt.c] and [runtime/backtrace_nat.c].
 *
 * [backtrace.c] has a unique implementation, and exposes a uniform
 * higher level API above [backtrace_{byt,nat}.c].
 */

/* Extract location information for the given raw_backtrace_slot */

struct caml_loc_info {
  int loc_valid;
  int loc_is_raise;
  char * loc_filename;
  int loc_lnum;
  int loc_startchr;
  int loc_endchr;
  int loc_is_inlined;
};

/* When compiling with -g, backtrace slots have debug info associated.
 * When a call is inlined in native mode, debuginfos form a sequence.
 */
typedef void * debuginfo;

/* Check availability of debug information before extracting a trace.
 * Relevant for bytecode, always true for native code. */
int caml_debug_info_available(void);

/* Return debuginfo associated to a slot or NULL. */
debuginfo caml_debuginfo_extract(backtrace_slot slot);

/* In case of an inlined call return next debuginfo or NULL otherwise. */
debuginfo caml_debuginfo_next(debuginfo dbg);

/* Extract locations from backtrace_slot */
void caml_debuginfo_location(debuginfo dbg, /*out*/ struct caml_loc_info * li);

/* In order to prevent the GC from walking through the debug
   information (which have no headers), we transform slots to 31/63 bits
   ocaml integers by shifting them by 1 to the right. We do not lose
   information as slots are aligned.

   In particular, we do not need to use [caml_modify] when setting
   an array element with such a value.
 */
#define Val_backtrace_slot(bslot) (Val_long(((uintnat)(bslot))>>1))
#define Backtrace_slot_val(vslot) ((backtrace_slot)(Long_val(vslot) << 1))

/* Allocate Caml_state->backtrace_buffer. Returns 0 on success, -1 otherwise */
int caml_alloc_backtrace_buffer(void);

#ifndef NATIVE_CODE
/* These two functions are used by the bytecode runtime when loading
   and unloading bytecode */
value caml_add_debug_info(code_t code_start, value code_size,
                                   value events_heap);
value caml_remove_debug_info(code_t start);
#endif

#define BACKTRACE_BUFFER_SIZE 1024

/* Besides decoding backtrace info, [backtrace_prim] has two other
 * responsibilities:
 *
 * It defines the [caml_stash_backtrace] function, which is called to quickly
 * fill the backtrace buffer by walking the stack when an exception is raised.
 *
 * It also defines the two following functions, which makes it possible
 * to store upto [max_frames_value] frames of the current call
 * stack. This is not used in an exception-raising context, but only
 * when the user requests to save the trace (hopefully less often), or
 * the context of profiling. Instead of using a bounded buffer as
 * [caml_stash_backtrace], we first traverse the stack to compute the
 * right size, then allocate space for the trace.
 *
 * The first function, [caml_current_callstack_size] computes the size
 * (in words) of the needed buffer, while the second actually writes
 * the call stack to the buffer as an object of type
 * [raw_backtrace]. It should always be called with a buffer of the
 * size predicted by [caml_current_callstack_size]. The reason we use
 * two separated functions is to allow using either [caml_alloc] (for
 * performance) or [caml_alloc_shr] (when we need to avoid a call to
 * the GC, in memprof.c).
 *
 * The alloc_idx parameter to [caml_current_callstack_write] is used
 * to select between the backtraces of different allocation sites which
 * were combined by Comballoc. Passing -1 here means the caller doesn't
 * care which is chosen.
 *
 * We use `intnat` for max_frames because, were it only `int`, passing
 * `max_int` from the OCaml side would overflow on 64bits machines. */

intnat caml_current_callstack_size(intnat max_frames);
void caml_current_callstack_write(value trace, int alloc_idx);

#endif /* CAML_INTERNALS */

#endif /* CAML_BACKTRACE_PRIM_H */
