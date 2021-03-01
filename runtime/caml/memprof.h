/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*            Jacques-Henri Jourdan, projet Gallium, INRIA Paris          */
/*                                                                        */
/*   Copyright 2016 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#ifndef CAML_MEMPROF_H
#define CAML_MEMPROF_H

#ifdef CAML_INTERNALS

#include "config.h"
#include "mlvalues.h"
#include "roots.h"

extern int caml_memprof_suspended;

extern value caml_memprof_handle_postponed_exn(void);
extern void caml_memprof_check_action_pending(void);

extern void caml_memprof_track_alloc_shr(value block);
extern void caml_memprof_track_young(uintnat wosize, int from_caml,
                                     int nallocs, unsigned char* alloc_lens);
extern void caml_memprof_track_interned(header_t* block, header_t* blockend);

extern void caml_memprof_renew_minor_sample(void);
extern value* caml_memprof_young_trigger;

extern void caml_memprof_oldify_young_roots(void);
extern void caml_memprof_minor_update(void);
extern void caml_memprof_do_roots(scanning_action f);
extern void caml_memprof_update_clean_phase(void);
extern void caml_memprof_invert_tracked(void);

extern void caml_memprof_shutdown(void);

#endif

#endif /* CAML_MEMPROF_H */
