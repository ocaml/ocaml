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

/* Extract location information for the given raw_backtrace_slot */

struct caml_loc_info {
  int loc_valid;
  int loc_is_raise;
  char * loc_filename;
  int loc_lnum;
  int loc_startchr;
  int loc_endchr;
};

int caml_debug_info_available(void);
void caml_extract_location_info(code_t pc, /*out*/ struct caml_loc_info * li);

/* Accessing raw_backtrace_slot values */
value caml_raw_backtrace_slot_of_code(code_t pc);
code_t caml_raw_backtrace_slot_code(value slot);

#define BACKTRACE_BUFFER_SIZE 1024

#endif /* CAML_BACKTRACE_PRIM_H */
