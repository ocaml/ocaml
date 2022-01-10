/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Damien Doligez, projet Para, INRIA Rocquencourt            */
/*                                                                        */
/*   Copyright 1997 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

/* Operations on weak arrays */

#ifndef CAML_WEAK_H
#define CAML_WEAK_H

#include "mlvalues.h"
#include "memory.h"

#ifdef __cplusplus
extern "C" {
#endif
extern value caml_ephe_none;

#ifdef CAML_INTERNALS

struct caml_ephe_info {
  value todo; /* These are ephemerons which need to be marked and swept in the
                 current cycle. If the ephemeron is alive, after marking, they
                 go into the live list after cleaning them off the unreachable
                 keys and releasing values if any of the keys are unreachable.
                 */
  value live; /* These are ephemerons which are alive in the current cycle,
                 whose keys and data are live (or not set). */
  uintnat cycle;
  struct {
    value* todop;
    uintnat cycle;
  } cursor;
};

/** The first field 0:  weak list;
       second field 1:  data;
       others       2..:  keys;

    A weak pointer is an ephemeron with the data at caml_ephe_none
    If fields are added, don't forget to update weak.ml, [additional_values],
    and obj.ml, [Ephemeron.additional_values].


 */

#define CAML_EPHE_LINK_OFFSET 0
#define CAML_EPHE_DATA_OFFSET 1
#define CAML_EPHE_FIRST_KEY 2
#define CAML_EPHE_MAX_WOSIZE (Max_wosize - CAML_EPHE_FIRST_KEY)

#define Ephe_link(e) (*(Op_val(e) + CAML_EPHE_LINK_OFFSET))
#define Ephe_data(e) (*(Op_val(e) + CAML_EPHE_DATA_OFFSET))

struct caml_ephe_info* caml_alloc_ephe_info (void);
void caml_ephe_clean(value e);

#endif /* CAML_INTERNALS */

#ifdef __cplusplus
}
#endif

#endif /* CAML_WEAK_H */
