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
  asize_t size;
  char *next;
} heap_chunk_head;

extern int gc_phase;
extern unsigned long allocated_words;
extern unsigned long extra_heap_memory;

#define Phase_mark 0
#define Phase_sweep 1

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

void init_major_heap P((asize_t));
asize_t round_heap_chunk_size P((asize_t));
void darken P((value));
void major_collection_slice P((void));
void major_collection P((void));
void finish_major_cycle P((void));


#endif /* _major_gc_ */
