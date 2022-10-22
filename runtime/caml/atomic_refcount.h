/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*      Florian Angeletti, projet Cambium, Inria                          */
/*                                                                        */
/*   Copyright 2022 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#ifndef CAML_ATOMIC_REFCOUNT_H
#define CAML_ATOMIC_REFCOUNT_H

#ifdef CAML_INTERNALS

#include "camlatomic.h"

Caml_inline void caml_atomic_refcount_init(atomic_uintnat* refc, uintnat n){
  atomic_store_rel(refc, n);
}

Caml_inline uintnat caml_atomic_refcount_decr(atomic_uintnat* refcount){
  return atomic_fetch_add (refcount, -1);
}

Caml_inline uintnat caml_atomic_refcount_incr(atomic_uintnat* refcount){
  return atomic_fetch_add (refcount, 1);
}

#endif /* CAML_INTERNALS */

#endif // CAML_ATOMIC_REFCOUNT_H
