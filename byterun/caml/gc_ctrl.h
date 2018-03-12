/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*              Damien Doligez, projet Para, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 1996 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#ifndef CAML_GC_CTRL_H
#define CAML_GC_CTRL_H

#ifdef CAML_INTERNALS

#include "misc.h"

extern uintnat
     caml_max_stack_size,
     caml_fiber_wsz;

uintnat caml_normalize_heap_increment (uintnat);

void caml_init_gc ();
value caml_gc_stat(value);

#ifdef DEBUG
void caml_heap_check (void);
#endif

#endif /* CAML_INTERNALS */

#endif /* CAML_GC_CTRL_H */
