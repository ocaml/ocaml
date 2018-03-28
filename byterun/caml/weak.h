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

#ifdef CAML_INTERNALS

#include "mlvalues.h"

#define CAML_EPHE_LINK_OFFSET 0
#define CAML_EPHE_DATA_OFFSET 1
#define CAML_EPHE_FIRST_KEY 2

extern value caml_weak_list_head;
extern value caml_weak_none;

#endif /* CAML_INTERNALS */

#endif /* CAML_WEAK_H */
