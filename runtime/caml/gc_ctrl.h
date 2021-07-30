/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*              Damien Doligez, projet Para, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 1996 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#ifndef CAML_GC_CTRL_H
#define CAML_GC_CTRL_H

#ifdef CAML_INTERNALS

#include "misc.h"

/* Global variables moved to Caml_state in 4.10 */
#define caml_stat_minor_words (Caml_state_field(stat_minor_words))
#define caml_stat_promoted_words (Caml_state_field(stat_promoted_words))
#define caml_stat_major_words (Caml_state_field(stat_major_words))
#define caml_stat_minor_collections (Caml_state_field(stat_minor_collections))
#define caml_stat_major_collections (Caml_state_field(stat_major_collections))
#define caml_stat_heap_wsz (Caml_state_field(stat_heap_wsz))
#define caml_stat_top_heap_wsz (Caml_state_field(stat_top_heap_wsz))
#define caml_stat_compactions (Caml_state_field(stat_compactions))
#define caml_stat_heap_chunks (Caml_state_field(stat_heap_chunks))

/*
  minor_size: cf. minor_heap_size in gc.mli
  major_size: Size in words of the initial major heap
  major_incr: cf. major_heap_increment in gc.mli
  percent_fr: cf. space_overhead in gc.mli
  percent_m : cf. max_overhead in gc.mli
  window    : cf. window_size in gc.mli
  custom_maj: cf. custom_major_ratio in gc.mli
  custom_min: cf. custom_minor_ratio in gc.mli
  custom_bsz: cf. custom_minor_max_size in gc.mli
  policy    : cf. allocation_policy in gc.mli
*/
void caml_init_gc (uintnat minor_size, uintnat major_size, uintnat major_incr,
                   uintnat percent_fr, uintnat percent_m, uintnat window,
                   uintnat custom_maj, uintnat custom_min, uintnat custom_bsz,
                   uintnat policy);


#ifdef DEBUG
void caml_heap_check (void);
#endif

#endif /* CAML_INTERNALS */

#endif /* CAML_GC_CTRL_H */
