/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*                  Damien Doligez, Jane Street Group, LLC                */
/*                                                                        */
/*   Copyright 2015 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#ifndef CAML_STARTUP_AUX_H
#define CAML_STARTUP_AUX_H

#ifdef CAML_INTERNALS

#include "config.h"

extern void caml_init_locale(void);
extern void caml_free_locale(void);

extern uintnat caml_init_max_stack_wsz;
extern uintnat caml_trace_level;
extern int caml_cleanup_on_exit;

/* Common entry point to the various caml_startup functions.
   Returns 0 if the runtime is already initialized.
   If [pooling] is 1, [caml_stat_*] functions will be backed by a pool
   that will be freed on caml_shutdown. */
extern int caml_startup_common(int pooling);

#endif /* CAML_INTERNALS */

#endif /* CAML_STARTUP_AUX_H */
