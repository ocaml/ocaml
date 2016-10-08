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

/* Backtrace generation is split in [backtrace.c] and [backtrace_prim.c].
 *
 * [backtrace_prim.c] contains all backend-specific code, and has two different
 * implementations in [byterun/backtrace_prim.c] and [asmrun/backtrace_prim.c].
 *
 * [backtrace.c] has a unique implementation, and expose a uniform
 * higher level API above [backtrace_prim.c].
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
 * When a call is inlined in native mode, debuginfos form a linked list.
 */
typedef void * debuginfo;

/* Check availability of debug information before extracting a trace.
 * Relevant for bytecode, always true for native code. */
int caml_debug_info_available(void);

/* Return debuginfo associated to a slot or NULL. */
debuginfo caml_debuginfo_extract(value slot);

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

   WARNING : In the case the slot refers to an allocation, the
   encoding of a slot as a value can actually be a pointer to a heap
   allocated pair of the slot and the allocation identifier. Hence,
   [Backtrace_slot_val] should only be used internally in the
   implementation of [caml_debuginfo_extract].
 */
#define Val_backtrace_slot(bslot) (Val_long(((uintnat)(bslot))>>1))
#define Backtrace_slot_val(vslot) ((backtrace_slot)(Long_val(vslot) << 1))

#define BACKTRACE_BUFFER_SIZE 1024

/* Besides decoding backtrace info, [backtrace_prim] has two other
 * responsibilities:
 *
 * It defines the [caml_stash_backtrace] function, which is called to quickly
 * fill the backtrace buffer by walking the stack when an exception is raised.
 *
 * It also defines the [caml_get_current_callstack_impl] function, which also
 * walks the stack but directly turns it into a [raw_backtrace] and is called
 * explicitly.
 *
 * If avoid_gc is set to non-0, then the block is allocated using
 * [alloc_shr], so that it is guaranteed to be in the major heap and
 * that no call to the GC is made.
 */

value caml_get_current_callstack_impl(intnat max_frames, int avoid_gc);

#endif /* CAML_INTERNALS */

#endif /* CAML_BACKTRACE_PRIM_H */
