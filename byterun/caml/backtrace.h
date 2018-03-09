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

#ifndef CAML_BACKTRACE_H
#define CAML_BACKTRACE_H

#include "mlvalues.h"
#include "memory.h"

CAMLprim value caml_record_backtrace(value vflag);
CAMLprim value caml_get_current_callstack(value max_frames);
CAMLprim value caml_get_continuation_callstack(value stack, value max_frames);
#ifndef NATIVE_CODE
extern void caml_stash_backtrace(value exn, code_t pc, value * sp, int reraise);
#endif
CAMLextern void caml_print_exception_backtrace(void);
CAMLextern void caml_init_debug_info(void);

#endif /* CAML_BACKTRACE_H */
