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
CAMLextern void (*caml_scan_roots_hook)(scanning_action, void*,
                                        caml_domain_state*);

CAMLextern void caml_do_roots (scanning_action f, void* data,
                               caml_domain_state* d, int do_final_val);
CAMLextern void caml_do_local_roots(scanning_action f, void* data,
                                    struct caml__roots_block* local_roots,
                                    struct stack_info *current_stack,
                                    value * v_gc_regs);

#endif /* CAML_INTERNALS */

#endif /* CAML_ROOTS_H */
