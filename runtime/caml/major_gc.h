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

#ifndef CAML_MAJOR_GC_H
#define CAML_MAJOR_GC_H

#ifdef CAML_INTERNALS

typedef enum {
  Phase_sweep_and_mark_main,
  Phase_mark_final,
  Phase_sweep_ephe
} gc_phase_t;
extern gc_phase_t caml_gc_phase;

Caml_inline char caml_gc_phase_char(gc_phase_t phase) {
  switch (phase) {
    case Phase_sweep_and_mark_main:
      return 'M';
    case Phase_mark_final:
      return 'F';
    case Phase_sweep_ephe:
      return 'E';
    default:
      return 'U';
  }
}

intnat caml_opportunistic_major_work_available (void);
void caml_opportunistic_major_collection_slice (intnat);
/* auto-triggered slice from within the GC */
#define AUTO_TRIGGERED_MAJOR_SLICE -1
/* external triggered slice, but GC will compute the amount of work */
#define GC_CALCULATE_MAJOR_SLICE 0
void caml_major_collection_slice (intnat);
void caml_finish_sweeping(void);
void caml_finish_marking (void);
int caml_init_major_gc(caml_domain_state*);
void caml_teardown_major_gc(void);
void caml_darken(void*, value, volatile value* ignored);
void caml_darken_cont(value);
void caml_mark_root(value, value*);
void caml_empty_mark_stack(void);
void caml_finish_major_cycle(void);

/* Ephemerons and finalisers */
void caml_orphan_allocated_words(void);
void caml_add_to_orphaned_ephe_list(struct caml_ephe_info* ephe_info);
void caml_add_orphaned_finalisers (struct caml_final_info*);
void caml_final_domain_terminate (caml_domain_state *domain_state);

/* Forces finalisation of all heap-allocated values,
   disregarding both local and global roots.

   Warning: finalisation is performed by means of forced sweeping, which may
   result in pointers referencing nonexistent values; therefore the function
   should only be used on runtime shutdown.
*/
void caml_finalise_heap (void);

/* This variable is only written with the world stopped,
   so it need not be atomic */
extern uintnat caml_major_cycles_completed;

double caml_mean_space_overhead(void);

#endif /* CAML_INTERNALS */

#endif /* CAML_MAJOR_GC_H */
