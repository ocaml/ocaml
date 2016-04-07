/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*         Xavier Leroy and Damien Doligez, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* To walk the memory roots for garbage collection */

#include "finalise.h"
#include "globroots.h"
#include "major_gc.h"
#include "memory.h"
#include "minor_gc.h"
#include "misc.h"
#include "mlvalues.h"
#include "roots.h"
#include "fiber.h"
#include "major_gc.h"
#include "shared_heap.h"
#include "fiber.h"

#ifdef NATIVE_CODE
#include "frame_descriptors.h"

/* Communication with [caml_start_program] and [caml_call_gc]. */

/* FIXME: there should be one of these per domain */

intnat caml_globals_inited = 0;
static intnat caml_globals_scanned = 0;

#endif

CAMLexport __thread struct caml__roots_block *caml_local_roots = NULL;

CAMLexport void caml_do_local_roots (scanning_action f, struct domain* domain)
{
  struct caml__roots_block *lr;
  int i, j;
  value* sp;

#ifdef NATIVE_CODE
  struct caml_domain_state* st = domain->state;
  caml_scan_stack_roots(f, st->bottom_of_stack,
                        st->last_return_address, st->gc_regs);
#else
  f(*(domain->current_stack), domain->current_stack);
#endif
  for (lr = *(domain->local_roots); lr != NULL; lr = lr->next) {
    for (i = 0; i < lr->ntables; i++){
      for (j = 0; j < lr->nitems; j++){
        sp = &(lr->tables[i][j]);
        if (*sp != 0) {
          f (*sp, sp);
        }
      }
    }
  }
}
