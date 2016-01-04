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

/* Check availability of debug information before extracting a trace.
 * Relevant for bytecode, always true for native code. */
int caml_debug_info_available(void);

/* Extract locations from backtrace_slot */
void caml_extract_location_info(backtrace_slot pc,
                                /*out*/ struct caml_loc_info * li);

/* Expose a [backtrace_slot] as a OCaml value of type [raw_backtrace_slot].
 * The value returned should be an immediate and not an OCaml block, so that it
 * is safe to store using direct assignment and [Field], and not [Store_field] /
 * [caml_modify].  */
value caml_val_raw_backtrace_slot(backtrace_slot pc);
backtrace_slot caml_raw_backtrace_slot_val(value slot);

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

#endif /* CAML_BACKTRACE_PRIM_H */
