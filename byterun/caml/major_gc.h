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

#ifndef CAML_MAJOR_GC
#define CAML_MAJOR_GC

#ifdef CAML_INTERNALS

intnat caml_major_collection_slice (intnat, intnat* left /* out */);
void caml_finish_sweeping(void);
void caml_finish_marking (void);
uintnat caml_get_num_domains_to_mark(void);
void caml_init_major_gc(void);
void caml_teardown_major_gc(void);
void caml_darken(void*, value, value* ignored);
void caml_mark_root(value, value*);
void caml_empty_mark_stack(void);
void caml_finish_major_cycle(void);


struct heap_stats {
  intnat pool_words;
  intnat pool_max_words;
  intnat pool_live_words;
  intnat pool_live_blocks;
  intnat pool_frag_words;
  intnat large_words;
  intnat large_max_words;
  intnat large_blocks;
};
void caml_accum_heap_stats(struct heap_stats* acc, const struct heap_stats* s);

struct gc_stats {
  uint64_t minor_words;
  uint64_t promoted_words;
  uint64_t major_words;
  uint64_t minor_collections;
  struct heap_stats major_heap;
};
void caml_sample_gc_stats(struct gc_stats* buf);

/* Forces finalisation of all heap-allocated values,
   disregarding both local and global roots.

   Warning: finalisation is performed by means of forced sweeping, which may
   result in pointers referencing nonexistent values; therefore the function
   should only be used on runtime shutdown.
*/
void caml_finalise_heap (void);

#endif /* CAML_INTERNALiS */

#endif /* CAML_MAJOR_GC_H */
