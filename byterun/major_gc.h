/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*             Damien Doligez, projet Para, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  Automatique.  Distributed only by permission.                      */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#ifndef _major_gc_
#define _major_gc_


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

extern int gc_phase;
extern unsigned long allocated_words;
extern unsigned long extra_heap_memory;

#define Phase_mark 0
#define Phase_sweep 1
#define Phase_idle 2

#ifdef __alpha
typedef int page_table_entry;
#else
typedef char page_table_entry;
#endif

extern char *heap_start;
extern char *heap_end;
extern unsigned long total_heap_size;
extern page_table_entry *page_table;
extern asize_t page_table_size;
extern char *gc_sweep_hp;

#define In_heap 1
#define Not_in_heap 0
#define Page(p) (((addr) (p) - (addr) heap_start) >> Page_log)
#define Is_in_heap(p) \
  ((addr)(p) >= (addr)heap_start && (addr)(p) < (addr)heap_end \
   && page_table [Page (p)])

void init_major_heap (asize_t);
asize_t round_heap_chunk_size (asize_t);
void darken (value, value *);
void major_collection_slice (void);
void major_collection (void);
void finish_major_cycle (void);


#endif /* _major_gc_ */
