/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*             Damien Doligez, projet Para, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  Distributed only by permission.                   */
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
     stat_heap_size = 0,              /* bytes */
     stat_compactions = 0;

extern asize_t major_heap_increment;  /* bytes; see major_gc.c */
extern unsigned long percent_free;    /*        see major_gc.c */
extern unsigned long percent_max;     /*        see compact.c */

#define Next(hp) ((hp) + Bhsize_hp (hp))

#ifdef DEBUG

/* Check that [v]'s header looks good.  [v] must be a block in the heap. */
static void check_head (value v)
{
  Assert (Is_block (v) && Is_in_heap (v));

  Assert (Wosize_val (v) != 0);
  Assert (Color_hd (Hd_val (v)) != Blue);
  Assert (Is_in_heap (v));
  if (Tag_val (v) == Infix_tag){
    int offset = Wsize_bsize (Infix_offset_val (v));
    value trueval = Val_op (&Field (v, -offset));
    Assert (Tag_val (trueval) == Closure_tag);
    Assert (Wosize_val (trueval) > offset);
    Assert (Is_in_heap (&Field (trueval, Wosize_val (trueval) - 1)));
  }else{
    Assert (Is_in_heap (&Field (v, Wosize_val (v) - 1)));
  }
  if (Tag_val (v) ==  Double_tag){
    Assert (Wosize_val (v) == Double_wosize);
  }else if (Tag_val (v) == Double_array_tag){
    Assert (Wosize_val (v) % Double_wosize == 0);
  }
}

static void check_block (char *hp)
{
  mlsize_t nfields = Wosize_hp (hp);
  mlsize_t i;
  value v = Val_hp (hp);
  value f;
  mlsize_t lastbyte;
  
  check_head (v);
  switch (Tag_hp (hp)){
  case Abstract_tag: break;
  case String_tag:
    /* not true when check_urgent_gc is called by alloc or alloc_string:
       lastbyte = Bosize_val (v) - 1;
       i = Byte (v, lastbyte);
       Assert (i >= 0 && i < sizeof (value));
       Assert (Byte (v, lastbyte - i) == 0);
    */
    break;
  case Double_tag:
    Assert (Wosize_val (v) == Double_wosize);
    break;
  case Double_array_tag:
    Assert (Wosize_val (v) % Double_wosize == 0);
    break;
  case Final_tag:
    Assert (!Is_in_heap (Final_fun (v)));
    break;
  
  case Infix_tag:
    Assert (0);
    break;

  default:
    Assert (Tag_hp (hp) < No_scan_tag);
    for (i = 0; i < Wosize_hp (hp); i++){
      f = Field (v, i);
      if (Is_block (f) && Is_in_heap (f)) check_head (f);
    }
  }
}

#endif /* DEBUG */

/* Check the heap structure (if compiled in debug mode) and
   gather statistics; return the stats if [returnstats] is true,
   otherwise return [Val_unit].
*/
static value heap_stats (int returnstats)
{
  long live_words = 0, live_blocks = 0,
       free_words = 0, free_blocks = 0, largest_free = 0,
       fragments = 0, heap_chunks = 0;
  char *chunk = heap_start, *chunk_end;
  char *cur_hp, *prev_hp;
  header_t cur_hd;

#ifdef DEBUG
  gc_message (0xFFFF, "### O'Caml runtime: heap check ###\n", 0);
#endif

  while (chunk != NULL){
    ++ heap_chunks;
    chunk_end = chunk + Chunk_size (chunk);
    prev_hp = NULL;
    cur_hp = chunk;
    while (cur_hp < chunk_end){
      cur_hd = Hd_hp (cur_hp);
                                           Assert (Next (cur_hp) <= chunk_end);
      switch (Color_hd (cur_hd)){
      case White:
        if (Wosize_hd (cur_hd) == 0){
          ++fragments;
          Assert (prev_hp == NULL
                  || (Color_hp (prev_hp) != Blue && Wosize_hp (prev_hp) > 0)
                  || cur_hp == gc_sweep_hp);
          Assert (Next (cur_hp) == chunk_end
                  || (Color_hp (Next (cur_hp)) != Blue
                      && Wosize_hp (Next (cur_hp)) > 0)
                  || Next (cur_hp) == gc_sweep_hp);
        }else{
          if (gc_phase == Phase_mark || gc_phase == Phase_idle){
            ++ live_blocks;
            live_words += Whsize_hd (cur_hd);
#ifdef DEBUG
            check_block (cur_hp);
#endif
          }
        }
        break;
      case Gray: case Black:
        Assert (Wosize_hd (cur_hd) > 0);
        ++ live_blocks;
        live_words += Whsize_hd (cur_hd);
#ifdef DEBUG
        check_block (cur_hp);
#endif
        break;
      case Blue:
        Assert (Wosize_hd (cur_hd) > 0);
        ++ free_blocks;
        free_words += Whsize_hd (cur_hd);
        if (Whsize_hd (cur_hd) > largest_free){
          largest_free = Whsize_hd (cur_hd);
        }
        Assert (prev_hp == NULL
                || (Color_hp (prev_hp) != Blue && Wosize_hp (prev_hp) > 0)
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

  if (returnstats){
    value res = alloc_small (14, 0);

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
  }else{
    return Val_unit;
  }
}

#ifdef DEBUG
void heap_check (void)
{
  heap_stats (0);
}
#endif

value gc_stat(value v) /* ML */
{
  Assert (v == Val_unit);
  return heap_stats (1);
}

value gc_get(value v) /* ML */
{
  value res;

  Assert (v == Val_unit);
  res = alloc_small (6, 0);
  Field (res, 0) = Wsize_bsize (Val_long (minor_heap_size));        /* s */
  Field (res, 1) = Wsize_bsize (Val_long (major_heap_increment));   /* i */
  Field (res, 2) = Val_long (percent_free);                         /* o */
  Field (res, 3) = Val_long (verb_gc);                              /* v */
  Field (res, 4) = Val_long (percent_max);                          /* O */
#ifndef NATIVE_CODE
  Field (res, 5) = Val_long (max_stack_size);                       /* l */
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

  verb_gc = Long_val (Field (v, 3));

#ifndef NATIVE_CODE
  change_max_stack_size (Long_val (Field (v, 5)));
#endif

  newpf = norm_pfree (Long_val (Field (v, 2)));
  if (newpf != percent_free){
    percent_free = newpf;
    gc_message (0x20, "New space overhead: %d%%\n", percent_free);
  }

  newpm = norm_pmax (Long_val (Field (v, 4)));
  if (newpm != percent_max){
    percent_max = newpm;
    gc_message (0x20, "New max overhead: %d%%\n", percent_max);
  }

  newheapincr = norm_heapincr (Bsize_wsize (Long_val (Field (v, 1))));
  if (newheapincr != major_heap_increment){
    major_heap_increment = newheapincr;
    gc_message (0x20, "New heap increment size: %luk bytes\n",
                major_heap_increment/1024);
  }

    /* Minor heap size comes last because it will trigger a minor collection
       (thus invalidating [v]) and it can raise [Out_of_memory]. */
  newminsize = norm_minsize (Bsize_wsize (Long_val (Field (v, 0))));
  if (newminsize != minor_heap_size){
    gc_message (0x20, "New minor heap size: %luk bytes\n", newminsize/1024);
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

void init_gc (unsigned long minor_size, unsigned long major_size,
              unsigned long major_incr, unsigned long percent_fr,
              unsigned long percent_m,  unsigned long verb)
{
  unsigned long major_heap_size = Bsize_wsize (norm_heapincr (major_size));

#ifdef DEBUG
  gc_message (0xFFFF, "### O'Caml runtime: debug mode "
#ifdef CPU_TYPE_STRING
                               "(" CPU_TYPE_STRING ") "
#endif
                                                       "###\n", 0);
#endif /* DEBUG */

  verb_gc = verb;
  set_minor_heap_size (Bsize_wsize (norm_minsize (minor_size)));
  major_heap_increment = Bsize_wsize (norm_heapincr (major_incr));
  percent_free = norm_pfree (percent_fr);
  percent_max = norm_pmax (percent_m);
  init_major_heap (major_heap_size);
  gc_message (0x20, "Initial minor heap size: %luk bytes\n",
              minor_heap_size / 1024);
  gc_message (0x20, "Initial major heap size: %luk bytes\n",
              major_heap_size / 1024);
  gc_message (0x20, "Initial space overhead: %lu%%\n", percent_free);
  gc_message (0x20, "Initial max overhead: %lu%%\n", percent_max);
  gc_message (0x20, "Initial heap increment: %luk bytes\n",
              major_heap_increment / 1024);
}
