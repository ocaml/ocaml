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

#ifndef CAML_COMPACT_H
#define CAML_COMPACT_H

#ifdef CAML_INTERNALS

#include "config.h"
#include "misc.h"
#include "mlvalues.h"

/* [caml_compact_heap] compacts the heap and optionally changes the
   allocation policy.
   if [new_allocation_policy] is -1, the policy is not changed.
*/
void caml_compact_heap (intnat new_allocation_policy);

void caml_compact_heap_maybe (void);
void caml_invert_root (value v, value *p);

#endif /* CAML_INTERNALS */

#endif /* CAML_COMPACT_H */
