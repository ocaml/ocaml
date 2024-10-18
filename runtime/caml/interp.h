/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 1996 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

/* The bytecode interpreter */

#ifndef CAML_INTERP_H
#define CAML_INTERP_H

#ifdef CAML_INTERNALS

#include "misc.h"
#include "mlvalues.h"

CAMLextern
value caml_bytecode_interpreter (code_t prog, asize_t prog_size,
                                 value initial_env, intnat initial_extra_args);

/* For backward compatibility */

Caml_inline value caml_interprete (code_t prog, asize_t prog_size)
{
  return caml_bytecode_interpreter(prog, prog_size, Atom(0), 0);
}

#endif /* CAML_INTERNALS */

#endif /* CAML_INTERP_H */
