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

#include "alloc.h"
#include "compact.h"
#include "gc.h"
#include "gc_ctrl.h"
#include "major_gc.h"
#include "minor_gc.h"
#include "misc.h"
#include "mlvalues.h"
#include "stacks.h"

#ifndef NATIVE_CODE
extern unsigned long max_stack_size;    /* defined in stacks.c */
#endif

long stat_minor_words = 0,
     stat_promoted_words = 0,
     stat_major_words = 0,
     stat_minor_collections = 0,
     stat_major_collections = 0,
     stat_heap_size = 0,           /* bytes */
     stat_compactions = 0;

extern asize_t major_heap_increment;  /* bytes; cf. major_gc.c */
extern unsigned long percent_free;    /*        cf. major_gc.c */
extern unsigned long percent_max;     /*        cf. compact.c */

#define Next(hp) ((hp) + Bhsize_hp (hp))

/* This will also thoroughly verify the heap if compiled in DEBUG mode. */

value gc_stat(value v) /* ML */
{
  value res;
  long live_words = 0, live_blocks = 0,
       free_words = 0, free_blocks = 0, largest_free = 0,
       fragments = 0, heap_chunks = 0;
  char *chunk = heap_start, *chunk_end;
  char *cur_hp, *prev_hp;
  header_t cur_hd;

  Assert (v == Val_unit);

  while (chunk != NULL){
    ++ heap_chunks;
    chunk_end = chunk + Chunk_size (chunk);
    prev_hp = NULL;
    cur_hp = chunk;
    while (cur_hp < chunk_end){
      cur_hd = Hd_hp (cur_hp);
      switch (Color_hd (cur_hd)){
      case White:
        if (Wosize_hd (cur_hd) == 0){
          ++fragments;
          Assert (prev_hp == NULL
                  || (Color_hp (prev_hp) != Blue
                      && Wosize_hp (prev_hp) > 0)
		  || cur_hp == gc_sweep_hp);
          Assert (Next (cur_hp) == chunk_end
                  || (Color_hp (Next (cur_hp)) != Blue
                      && Wosize_hp (Next (cur_hp)) > 0)
		  || Next (cur_hp) == gc_sweep_hp);
          break;
        }
        /* FALLTHROUGH */
      case Gray: case Black:
        Assert (Wosize_hd (cur_hd) > 0);
        ++ live_blocks;
        live_words += Whsize_hd (cur_hd);
        break;
      case Blue:
        Assert (Wosize_hd (cur_hd) > 0);
        ++ free_blocks;
        free_words += Whsize_hd (cur_hd);
        if (Whsize_hd (cur_hd) > largest_free){
          largest_free = Whsize_hd (cur_hd);
        }
        Assert (prev_hp == NULL
                || (Color_hp (prev_hp) != Blue
                    && Wosize_hp (prev_hp) > 0)
		|| cur_hp == gc_sweep_hp);
        Assert (Next (cur_hp) == chunk_end
                || (Color_hp (Next (cur_hp)) != Blue
                    && Wosize_hp (Next (cur_hp)) > 0)
		|| Next (cur_hp) == gc_sweep_hp);
        break;
      }
      prev_hp = cur_hp;
      cur_hp = Next (cur_hp);
    }                                          Assert (cur_hp == chunk_end);
    chunk = Chunk_next (chunk);
  }
  
  Assert (live_words + free_words + fragments == Wsize_bsize (stat_heap_size));

  res = alloc (14, 0);
  Field (res, 0) = Val_long (stat_minor_words
                             + Wsize_bsize (young_end - young_ptr));
  Field (res, 1) = Val_long (stat_promoted_words);
  Field (res, 2) = Val_long (stat_major_words + allocated_words);
  Field (res, 3) = Val_long (stat_minor_collections);
  Field (res, 4) = Val_long (stat_major_collections);
  Field (res, 5) = Val_long (Wsize_bsize (stat_heap_size));
  Field (res, 6) = Val_long (heap_chunks);
  Field (res, 7) = Val_long (live_words);
  Field (res, 8) = Val_long (live_blocks);
  Field (res, 9) = Val_long (free_words);
  Field (res, 10) = Val_long (free_blocks);
  Field (res, 11) = Val_long (largest_free);
  Field (res, 12) = Val_long (fragments);
  Field (res, 13) = Val_long (stat_compactions);
  return res;
}

value gc_get(value v) /* ML */
{
  value res;

  Assert (v == Val_unit);
  res = alloc (6, 0);
  Field (res, 0) = Wsize_bsize (Val_long (minor_heap_size));
  Field (res, 1) = Wsize_bsize (Val_long (major_heap_increment));
  Field (res, 2) = Val_long (percent_free);
  Field (res, 3) = Val_bool (verb_gc);
  Field (res, 4) = Val_long (percent_max);
#ifndef NATIVE_CODE
  Field (res, 5) = Val_long (max_stack_size);
#else
  Field (res, 5) = 0;
#endif
  return res;
}

#define Max(x,y) ((x) < (y) ? (y) : (x))

static unsigned long norm_pfree (long unsigned int p)
{
  return Max (p, 1);
}

static unsigned long norm_pmax (long unsigned int p)
{
  return p;
}

static long norm_heapincr (long unsigned int i)
{
#define Psv (Wsize_bsize (Page_size))
  i = ((i + Psv - 1) / Psv) * Psv;
  if (i < Heap_chunk_min) i = Heap_chunk_min;
  if (i > Heap_chunk_max) i = Heap_chunk_max;
  return i;
}

static long norm_minsize (long int s)
{
  if (s < Minor_heap_min) s = Minor_heap_min;
  if (s > Minor_heap_max) s = Minor_heap_max;
  return s;
}

value gc_set(value v) /* ML */
{
  unsigned long newpf, newpm;
  asize_t newheapincr;
  asize_t newminsize;

  verb_gc = Bool_val (Field (v, 3));

#ifndef NATIVE_CODE
  change_max_stack_size (Long_val (Field (v, 5)));
#endif

  newpf = norm_pfree (Long_val (Field (v, 2)));
  if (newpf != percent_free){
    percent_free = newpf;
    gc_message ("New space overhead: %d%%\n", percent_free);
  }

  newpm = norm_pmax (Long_val (Field (v, 4)));
  if (newpm != percent_max){
    percent_max = newpm;
    gc_message ("New max overhead: %d%%\n", percent_max);
  }

  newheapincr = norm_heapincr (Bsize_wsize (Long_val (Field (v, 1))));
  if (newheapincr != major_heap_increment){
    major_heap_increment = newheapincr;
    gc_message ("New heap increment size: %luk bytes\n",
                major_heap_increment/1024);
  }

    /* Minor heap size comes last because it will trigger a minor collection
       (thus invalidating [v]) and it can raise [Out_of_memory]. */
  newminsize = norm_minsize (Bsize_wsize (Long_val (Field (v, 0))));
  if (newminsize != minor_heap_size){
    gc_message ("New minor heap size: %luk bytes\n", newminsize/1024);
    set_minor_heap_size (newminsize);
  }
  return Val_unit;
}

value gc_minor(value v) /* ML */
{                                                    Assert (v == Val_unit);
  minor_collection ();
  return Val_unit;
}

value gc_major(value v) /* ML */
{                                                    Assert (v == Val_unit);
  minor_collection ();
  finish_major_cycle ();
  return Val_unit;
}

value gc_full_major(value v) /* ML */
{                                                    Assert (v == Val_unit);
  minor_collection ();
  finish_major_cycle ();
  finish_major_cycle ();
  return Val_unit;
}

value gc_compaction(value v) /* ML */
{                                                    Assert (v == Val_unit);
  minor_collection ();
  finish_major_cycle ();
  finish_major_cycle ();
  compact_heap ();
  return Val_unit;
}

void init_gc (long unsigned int minor_size, long unsigned int major_size, long unsigned int major_incr, long unsigned int percent_fr, long unsigned int percent_m, long unsigned int verb)
{
  unsigned long major_heap_size = Bsize_wsize (norm_heapincr (major_size));
#ifdef DEBUG
  verb_gc = 1;
  gc_message ("*** O'Caml runtime: debug mode ***\n", 0);
#endif
  verb_gc = verb;
  set_minor_heap_size (Bsize_wsize (norm_minsize (minor_size)));
  major_heap_increment = Bsize_wsize (norm_heapincr (major_incr));
  percent_free = norm_pfree (percent_fr);
  percent_max = norm_pmax (percent_m);
  init_major_heap (major_heap_size);
  gc_message ("Initial minor heap size: %luk bytes\n", minor_heap_size / 1024);
  gc_message ("Initial major heap size: %luk bytes\n", major_heap_size / 1024);
  gc_message ("Initial space overhead: %lu%%\n", percent_free);
  gc_message ("Initial max overhead: %lu%%\n", percent_max);
  gc_message ("Initial heap increment: %luk bytes\n",
              major_heap_increment / 1024);
}
