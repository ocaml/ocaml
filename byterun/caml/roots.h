/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*          Xavier Leroy and Damien Doligez, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 1996 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#ifndef CAML_ROOTS_H
#define CAML_ROOTS_H

#ifdef CAML_INTERNALS

#include "misc.h"
#include "memory.h"

typedef void (*scanning_action) (void*, value, value *);
CAMLexport void (*caml_scan_roots_hook)(scanning_action, void*, struct domain*);

void caml_do_local_roots(scanning_action, void*, struct domain*);

#endif /* CAML_INTERNALS */

#endif /* CAML_ROOTS_H */
