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

#define CAML_INTERNALS

#include "caml/domain_state.h"
#include "caml/memory.h"

CAMLexport caml_domain_state* Caml_state;

void caml_init_domain ()
{
  Caml_state = (caml_domain_state*)caml_stat_alloc_noexc(sizeof(caml_domain_state));
  if (Caml_state == NULL) {
    caml_fatal_error ("cannot initialize domain state");
  }
}
