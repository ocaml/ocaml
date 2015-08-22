/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 2001 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

#ifndef CAML_BACKTRACE_PRIM_H
#define CAML_BACKTRACE_PRIM_H

#include "backtrace.h"

/* Backtrace generation is split in the two files [backtrace.c] and [backtrace_prim.c].
 *
 * [backtrace_prim.c] contains all backend-specific code, and has two different
 * implementations in [byterun/backtrace_prim.c] and [asmrun/backtrace_prim.c].
 *
 * [backtrace.c] has a unique implementation, and rely on [backtrace_prim.c] interface
 * to expose a uniform higher level API.
 *
 * This file, [backtrace_prim.h] documents the interface expected by [backtrace.c].
 */

/* Extract location information for the given raw_backtrace_slot */

struct caml_loc_info {
  int loc_valid;
  int loc_is_raise;
  char * loc_filename;
  int loc_lnum;
  int loc_startchr;
  int loc_endchr;
};

/* Check availability of debug information before extracting a trace.
 * Relevant for bytecode, always true for native code. */
int caml_debug_info_available(void);

/* Extract locations from backtrace_slot */
void caml_extract_location_info(backtrace_slot pc, /*out*/ struct caml_loc_info * li);

/* Expose a [backtrace_slot] as an OCaml value. */
value caml_val_raw_backtrace_slot(backtrace_slot pc);
backtrace_slot caml_raw_backtrace_slot_val(value slot);

#define BACKTRACE_BUFFER_SIZE 1024

#endif /* CAML_BACKTRACE_PRIM_H */
