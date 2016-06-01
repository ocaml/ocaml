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

#ifndef CAML_ROOTS_H
#define CAML_ROOTS_H

#include "misc.h"
#include "memory.h"

typedef void (*scanning_action) (value, value *);

void caml_do_local_roots(scanning_action, struct domain*);
void caml_do_sampled_roots(scanning_action, struct domain*);


#endif /* CAML_ROOTS_H */
