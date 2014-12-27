/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*             Damien Doligez, projet Para, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* Free lists of heap blocks. */

#ifndef CAML_FREELIST_H
#define CAML_FREELIST_H


#include "misc.h"
#include "mlvalues.h"

extern asize_t caml_fl_cur_wsz;

header_t *caml_fl_allocate (mlsize_t wo_sz);
void caml_fl_init_merge (void);
void caml_fl_reset (void);
header_t *caml_fl_merge_block (value);
void caml_fl_add_blocks (value);
void caml_make_free_blocks (value *, mlsize_t wsz, int, int);
void caml_set_allocation_policy (uintnat);


#endif /* CAML_FREELIST_H */
