/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*            Damien Doligez, projet Para, INRIA Rocquencourt          */
/*                                                                     */
/*  Copyright 1997 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* Operations on weak arrays */

#ifndef CAML_WEAK_H
#define CAML_WEAK_H

#include "mlvalues.h"

extern value caml_ephe_list_head;
extern value caml_ephe_none;

void caml_ephe_clean (value v);

#endif /* CAML_WEAK_H */
