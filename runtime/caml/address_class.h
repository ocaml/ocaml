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

/* Classification of addresses for GC and runtime purposes. */

/* Multicore runtime supports only the "no naked pointers" mode where any
   out-of-heap pointers are not observable by the GC. The out-of-heap pointers
   are either:

   - wrapped in Abstract_tag or Custom_tag objects, or
   - have a valid header with colour `NOT_MARKABLE`, or
   - made to look like immediate values by tagging the least significant bit so
     that the GC does not follow it. This strategy has the downside that
     out-of-heap pointers may not point to odd addresses.

   A valid value is either:
   - a tagged integer (Is_long)
   - a pointer to the minor heap
   - a pointer to a well-formed block outside the minor heap. It may be in the
     major heap, or static data allocated by the OCaml code or the OCaml
     runtime, or a foreign pointer.

   To create a well-formed block outside the heap that the GC will not scan,
   one can use the Caml_out_of_heap_header from mlvalues.h.
*/

#ifndef CAML_ADDRESS_CLASS_H
#define CAML_ADDRESS_CLASS_H

#include "config.h"
#include "misc.h"
#include "mlvalues.h"

CAMLextern uintnat caml_minor_heaps_start;
CAMLextern uintnat caml_minor_heaps_end;

/* Is_young(val) is true iff val is in the reserved area for minor heaps */

#define Is_young(val) \
  (CAMLassert (Is_block (val)), \
   (char *)(val) < (char *)caml_minor_heaps_end && \
   (char *)(val) > (char *)caml_minor_heaps_start)

#define Is_block_and_young(val) (Is_block(val) && Is_young(val))

/* These definitions are retained for backwards compatibility with OCaml 4 */
#define Is_in_heap_or_young(a) 1
#define Is_in_value_area(a) 1

#endif /* CAML_ADDRESS_CLASS_H */
