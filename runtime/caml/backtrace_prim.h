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
  char * loc_defname;
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

/* Check load status of debug information for the main program. This is always 1
 * for native code. For bytecode, it is 1 if the debug information has been
 * loaded, 0 if it has not been loaded or one of the error constants in
 * startup.h if something went wrong loading the debug information. */
int caml_debug_info_status(void);

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

void caml_free_backtrace_buffer(backtrace_slot *backtrace_buffer);

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
 * It also defines the [caml_get_current_callstack] OCaml primitive, which also
 * walks the stack but directly turns it into a [raw_backtrace] and is called
 * explicitly.
 */

#endif /* CAML_INTERNALS */

#endif /* CAML_BACKTRACE_PRIM_H */
