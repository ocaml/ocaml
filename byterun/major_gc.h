/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*             Damien Doligez, projet Para, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#ifndef CAML_MAJOR_GC_H
#define CAML_MAJOR_GC_H


#include "freelist.h"
#include "misc.h"

typedef struct {
  void *block;           /* address of the malloced block this chunk live in */
  asize_t alloc;         /* in bytes, used for compaction */
  asize_t size;          /* in bytes */
  char *next;
} heap_chunk_head;

#define Chunk_size(c) (((heap_chunk_head *) (c)) [-1]).size
#define Chunk_alloc(c) (((heap_chunk_head *) (c)) [-1]).alloc
#define Chunk_next(c) (((heap_chunk_head *) (c)) [-1]).next
#define Chunk_block(c) (((heap_chunk_head *) (c)) [-1]).block

extern int caml_gc_phase;
extern unsigned long caml_allocated_words;
extern double caml_extra_heap_resources;
extern unsigned long caml_dependent_size, caml_dependent_allocated;
extern unsigned long caml_fl_size_at_phase_change;

#define Phase_mark 0
#define Phase_sweep 1
#define Phase_idle 2

#ifdef __alpha
typedef int page_table_entry;
#else
typedef char page_table_entry;
#endif

CAMLextern char *caml_heap_start;
CAMLextern char *caml_heap_end;
extern unsigned long total_heap_size;
CAMLextern page_table_entry *caml_page_table;
extern asize_t caml_page_low, caml_page_high;
extern char *caml_gc_sweep_hp;

#define In_heap 1
#define Not_in_heap 0
#define Page(p) ((unsigned long) (p) >> Page_log)
#define Is_in_heap(p) \
  (Assert (Is_block ((value) (p))), \
   (addr)(p) >= (addr)caml_heap_start && (addr)(p) < (addr)caml_heap_end \
   && caml_page_table [Page (p)])

void caml_init_major_heap (asize_t);           /* size in bytes */
asize_t caml_round_heap_chunk_size (asize_t);  /* size in bytes */
void caml_darken (value, value *);
long caml_major_collection_slice (long);
void major_collection (void);
void caml_finish_major_cycle (void);


#endif /* CAML_MAJOR_GC_H */
