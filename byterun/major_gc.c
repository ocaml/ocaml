/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
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

#include <limits.h>

#include "compact.h"
#include "custom.h"
#include "config.h"
#include "fail.h"
#include "finalise.h"
#include "freelist.h"
#include "gc.h"
#include "gc_ctrl.h"
#include "major_gc.h"
#include "misc.h"
#include "mlvalues.h"
#include "roots.h"
#include "weak.h"

int heap_profiling = 0;
void really_dump_heap();

uintnat caml_percent_free;
uintnat caml_major_heap_increment;
CAMLexport char *caml_heap_start;
char *caml_gc_sweep_hp;
int caml_gc_phase;        /* always Phase_mark, Phase_sweep, or Phase_idle */
static value *gray_vals;
static value *gray_vals_cur, *gray_vals_end;
static asize_t gray_vals_size;
static int heap_is_pure;   /* The heap is pure if the only gray objects
                              below [markhp] are also in [gray_vals]. */
uintnat caml_allocated_words;
uintnat caml_dependent_size, caml_dependent_allocated;
double caml_extra_heap_resources;
uintnat caml_fl_size_at_phase_change = 0;

extern char *caml_fl_merge;  /* Defined in freelist.c. */

static char *markhp, *chunk, *limit;

int caml_gc_subphase;     /* Subphase_{main,weak1,weak2,final} */
static value *weak_prev;

#ifdef DEBUG
static unsigned long major_gc_counter = 0;
#endif

static void realloc_gray_vals (void)
{
  value *new;

  Assert (gray_vals_cur == gray_vals_end);
  if (gray_vals_size < caml_stat_heap_size / 128){
    caml_gc_message (0x08, "Growing gray_vals to %"
                           ARCH_INTNAT_PRINTF_FORMAT "uk bytes\n",
                     (intnat) gray_vals_size * sizeof (value) / 512);
    new = (value *) realloc ((char *) gray_vals,
                             2 * gray_vals_size * sizeof (value));
    if (new == NULL){
      caml_gc_message (0x08, "No room for growing gray_vals\n", 0);
      gray_vals_cur = gray_vals;
      heap_is_pure = 0;
    }else{
      gray_vals = new;
      gray_vals_cur = gray_vals + gray_vals_size;
      gray_vals_size *= 2;
      gray_vals_end = gray_vals + gray_vals_size;
    }
  }else{
    gray_vals_cur = gray_vals + gray_vals_size / 2;
    heap_is_pure = 0;
  }
}

void caml_darken (value v, value *p /* not used */)
{
  if (Is_block (v) && Is_in_heap (v)) {
    header_t h = Hd_val (v);
    tag_t t = Tag_hd (h);
    if (t == Infix_tag){
      v -= Infix_offset_val(v);
      h = Hd_val (v);
      t = Tag_hd (h);
    }
    CAMLassert (!Is_blue_hd (h));
    if (Is_white_hd (h)){
      if (t < No_scan_tag){
        Hd_val (v) = Grayhd_hd (h);
        *gray_vals_cur++ = v;
        if (gray_vals_cur >= gray_vals_end) realloc_gray_vals ();
      }else{
        Hd_val (v) = Blackhd_hd (h);
      }
    }
  }
}

static void start_cycle (void)
{
  Assert (caml_gc_phase == Phase_idle);
  Assert (gray_vals_cur == gray_vals);
  caml_gc_message (0x01, "Starting new major GC cycle\n", 0);
  caml_darken_all_roots();
  caml_gc_phase = Phase_mark;
  caml_gc_subphase = Subphase_main;
  markhp = NULL;
#ifdef DEBUG
  ++ major_gc_counter;
  caml_heap_check ();
#endif
}

static void mark_slice (intnat work)
{
  value *gray_vals_ptr;  /* Local copy of gray_vals_cur */
  value v, child;
  header_t hd;
  mlsize_t size, i;

  caml_gc_message (0x40, "Marking %ld words\n", work);
  caml_gc_message (0x40, "Subphase = %ld\n", caml_gc_subphase);
  gray_vals_ptr = gray_vals_cur;
  while (work > 0){
    if (gray_vals_ptr > gray_vals){
      v = *--gray_vals_ptr;
      hd = Hd_val(v);
      Assert (Is_gray_hd (hd));
      Hd_val (v) = Blackhd_hd (hd);
      size = Wosize_hd (hd);
      if (Tag_hd (hd) < No_scan_tag){
        for (i = 0; i < size; i++){
          child = Field (v, i);
          if (Is_block (child) && Is_in_heap (child)) {
            hd = Hd_val (child);
            if (Tag_hd (hd) == Forward_tag){
              value f = Forward_val (child);
              if (Is_block (f)
                  && (!Is_in_value_area(f) || Tag_val (f) == Forward_tag
                      || Tag_val (f) == Lazy_tag || Tag_val (f) == Double_tag)){
                /* Do not short-circuit the pointer. */
              }else{
                Field (v, i) = f;
              }
            }
            else if (Tag_hd(hd) == Infix_tag) {
              child -= Infix_offset_val(child);
              hd = Hd_val(child);
            }
            if (Is_white_hd (hd)){
              Hd_val (child) = Grayhd_hd (hd);
              *gray_vals_ptr++ = child;
              if (gray_vals_ptr >= gray_vals_end) {
                gray_vals_cur = gray_vals_ptr;
                realloc_gray_vals ();
                gray_vals_ptr = gray_vals_cur;
              }
            }
          }
        }
      }
      work -= Whsize_wosize(size);
    }else if (markhp != NULL){
      if (markhp == limit){
        chunk = Chunk_next (chunk);
        if (chunk == NULL){
          markhp = NULL;
        }else{
          markhp = chunk;
          limit = chunk + Chunk_size (chunk);
        }
      }else{
        if (Is_gray_val (Val_hp (markhp))){
          Assert (gray_vals_ptr == gray_vals);
          *gray_vals_ptr++ = Val_hp (markhp);
        }
        markhp += Bhsize_hp (markhp);
      }
    }else if (!heap_is_pure){
      heap_is_pure = 1;
      chunk = caml_heap_start;
      markhp = chunk;
      limit = chunk + Chunk_size (chunk);
    }else{
      switch (caml_gc_subphase){
      case Subphase_main: {
        /* The main marking phase is over.  Start removing weak pointers to
           dead values. */
        caml_gc_subphase = Subphase_weak1;
        weak_prev = &caml_weak_list_head;
      }
        break;
      case Subphase_weak1: {
        value cur, curfield;
        mlsize_t sz, i;
        header_t hd;

        cur = *weak_prev;
        if (cur != (value) NULL){
          hd = Hd_val (cur);
          sz = Wosize_hd (hd);
          for (i = 1; i < sz; i++){
            curfield = Field (cur, i);
          weak_again:
            if (curfield != caml_weak_none
                && Is_block (curfield) && Is_in_heap (curfield)){
              if (Tag_val (curfield) == Forward_tag){
                value f = Forward_val (curfield);
                if (Is_block (f)) {
                  if (!Is_in_value_area(f) || Tag_val (f) == Forward_tag
                      || Tag_val (f) == Lazy_tag || Tag_val (f) == Double_tag){
                    /* Do not short-circuit the pointer. */
                  }else{
                    Field (cur, i) = curfield = f;
                    goto weak_again;
                  }
                }
              }
              if (Is_white_val (curfield)){
                Field (cur, i) = caml_weak_none;
              }
            }
          }
          weak_prev = &Field (cur, 0);
          work -= Whsize_hd (hd);
        }else{
          /* Subphase_weak1 is done.
             Handle finalised values and start removing dead weak arrays. */
          gray_vals_cur = gray_vals_ptr;
          caml_final_update ();
          gray_vals_ptr = gray_vals_cur;
          caml_gc_subphase = Subphase_weak2;
          weak_prev = &caml_weak_list_head;
        }
      }
        break;
      case Subphase_weak2: {
        value cur;
        header_t hd;

        cur = *weak_prev;
        if (cur != (value) NULL){
          hd = Hd_val (cur);
          if (Color_hd (hd) == Caml_white){
            /* The whole array is dead, remove it from the list. */
            *weak_prev = Field (cur, 0);
          }else{
            weak_prev = &Field (cur, 0);
          }
          work -= 1;
        }else{
          /* Subphase_weak2 is done.  Go to Subphase_final. */
          caml_gc_subphase = Subphase_final;
        }
      }
        break;
      case Subphase_final: {
        /* Initialise the sweep phase. */
        gray_vals_cur = gray_vals_ptr;
        caml_gc_sweep_hp = caml_heap_start;
        caml_fl_init_merge ();
        caml_gc_phase = Phase_sweep;
        chunk = caml_heap_start;
        caml_gc_sweep_hp = chunk;
        limit = chunk + Chunk_size (chunk);
        work = 0;
        caml_fl_size_at_phase_change = caml_fl_cur_size;
	if(heap_profiling) really_dump_heap();
      }
        break;
      default: Assert (0);
      }
    }
  }
  gray_vals_cur = gray_vals_ptr;
}

static void sweep_slice (intnat work)
{
  char *hp;
  header_t hd;

  caml_gc_message (0x40, "Sweeping %ld words\n", work);
  while (work > 0){
    if (caml_gc_sweep_hp < limit){
      hp = caml_gc_sweep_hp;
      hd = Hd_hp (hp);
      work -= Whsize_hd (hd);
      caml_gc_sweep_hp += Bhsize_hd (hd);
      switch (Color_hd (hd)){
      case Caml_white:
        if (Tag_hd (hd) == Custom_tag){
          void (*final_fun)(value) = Custom_ops_val(Val_hp(hp))->finalize;
          if (final_fun != NULL) final_fun(Val_hp(hp));
        }
        caml_gc_sweep_hp = caml_fl_merge_block_loc (Bp_hp (hp), PROF_SWEEP_SLICE);
        break;
      case Caml_blue:
        /* Only the blocks of the free-list are blue.  See [freelist.c]. */
        caml_fl_merge = Bp_hp (hp);
        break;
      default:          /* gray or black */
        Assert (Color_hd (hd) == Caml_black);
        Hd_hp (hp) = Whitehd_hd (hd);
        break;
      }
      Assert (caml_gc_sweep_hp <= limit);
    }else{
      chunk = Chunk_next (chunk);
      if (chunk == NULL){
        /* Sweeping is done. */
        ++ caml_stat_major_collections;
        work = 0;
        caml_gc_phase = Phase_idle;
      }else{
        caml_gc_sweep_hp = chunk;
        limit = chunk + Chunk_size (chunk);
      }
    }
  }
}

/* The main entry point for the GC.  Called after each minor GC.
   [howmuch] is the amount of work to do, 0 to let the GC compute it.
   Return the computed amount of work to do.
 */
intnat caml_major_collection_slice (intnat howmuch)
{
  double p, dp;
  intnat computed_work;
  /*
     Free memory at the start of the GC cycle (garbage + free list) (assumed):
                 FM = caml_stat_heap_size * caml_percent_free
                      / (100 + caml_percent_free)

     Assuming steady state and enforcing a constant allocation rate, then
     FM is divided in 2/3 for garbage and 1/3 for free list.
                 G = 2 * FM / 3
     G is also the amount of memory that will be used during this cycle
     (still assuming steady state).

     Proportion of G consumed since the previous slice:
                 PH = caml_allocated_words / G
                    = caml_allocated_words * 3 * (100 + caml_percent_free)
                      / (2 * caml_stat_heap_size * caml_percent_free)
     Proportion of extra-heap resources consumed since the previous slice:
                 PE = caml_extra_heap_resources
     Proportion of total work to do in this slice:
                 P  = max (PH, PE)
     Amount of marking work for the GC cycle:
                 MW = caml_stat_heap_size * 100 / (100 + caml_percent_free)
     Amount of sweeping work for the GC cycle:
                 SW = caml_stat_heap_size

     In order to finish marking with a non-empty free list, we will
     use 40% of the time for marking, and 60% for sweeping.

     If TW is the total work for this cycle,
                 MW = 40/100 * TW
                 SW = 60/100 * TW

     Amount of work to do for this slice:
                 W  = P * TW

     Amount of marking work for a marking slice:
                 MS = P * MW / (40/100)
                 MS = P * caml_stat_heap_size * 250 / (100 + caml_percent_free)
     Amount of sweeping work for a sweeping slice:
                 SS = P * SW / (60/100)
                 SS = P * caml_stat_heap_size * 5 / 3

     This slice will either mark MS words or sweep SS words.
  */
  if (caml_gc_phase == Phase_idle) start_cycle ();

  p = (double) caml_allocated_words * 3.0 * (100 + caml_percent_free)
      / Wsize_bsize (caml_stat_heap_size) / caml_percent_free / 2.0;
  if (caml_dependent_size > 0){
    dp = (double) caml_dependent_allocated * (100 + caml_percent_free)
         / caml_dependent_size / caml_percent_free;
  }else{
    dp = 0.0;
  }
  if (p < dp) p = dp;
  if (p < caml_extra_heap_resources) p = caml_extra_heap_resources;

  caml_gc_message (0x40, "allocated_words = %"
                         ARCH_INTNAT_PRINTF_FORMAT "u\n",
                   caml_allocated_words);
  caml_gc_message (0x40, "extra_heap_resources = %"
                         ARCH_INTNAT_PRINTF_FORMAT "uu\n",
                   (uintnat) (caml_extra_heap_resources * 1000000));
  caml_gc_message (0x40, "amount of work to do = %"
                         ARCH_INTNAT_PRINTF_FORMAT "uu\n",
                   (uintnat) (p * 1000000));

  if (caml_gc_phase == Phase_mark){
    computed_work = (intnat) (p * Wsize_bsize (caml_stat_heap_size) * 250
                              / (100 + caml_percent_free));
  }else{
    computed_work = (intnat) (p * Wsize_bsize (caml_stat_heap_size) * 5 / 3);
  }
  caml_gc_message (0x40, "ordered work = %ld words\n", howmuch);
  caml_gc_message (0x40, "computed work = %ld words\n", computed_work);
  if (howmuch == 0) howmuch = computed_work;
  if (caml_gc_phase == Phase_mark){
    mark_slice (howmuch);
    caml_gc_message (0x02, "!", 0);
  }else{
    Assert (caml_gc_phase == Phase_sweep);
    sweep_slice (howmuch);
    caml_gc_message (0x02, "$", 0);
  }

  if (caml_gc_phase == Phase_idle) caml_compact_heap_maybe ();

  caml_stat_major_words += caml_allocated_words;
  caml_allocated_words = 0;
  caml_dependent_allocated = 0;
  caml_extra_heap_resources = 0.0;
  return computed_work;
}

/* The minor heap must be empty when this function is called;
   the minor heap is empty when this function returns.
*/
/* This does not call caml_compact_heap_maybe because the estimations of
   free and live memory are only valid for a cycle done incrementally.
   Besides, this function is called by caml_compact_heap_maybe.
*/
void caml_finish_major_cycle (void)
{
  if (caml_gc_phase == Phase_idle) start_cycle ();
  while (caml_gc_phase == Phase_mark) mark_slice (LONG_MAX);
  Assert (caml_gc_phase == Phase_sweep);
  while (caml_gc_phase == Phase_sweep) sweep_slice (LONG_MAX);
  Assert (caml_gc_phase == Phase_idle);
  caml_stat_major_words += caml_allocated_words;
  caml_allocated_words = 0;
}

/* Make sure the request is at least Heap_chunk_min and round it up
   to a multiple of the page size.
*/
static asize_t clip_heap_chunk_size (asize_t request)
{
  if (request < Bsize_wsize (Heap_chunk_min)){
    request = Bsize_wsize (Heap_chunk_min);
  }
  return ((request + Page_size - 1) >> Page_log) << Page_log;
}

/* Make sure the request is >= caml_major_heap_increment, then call
   clip_heap_chunk_size, then make sure the result is >= request.
*/
asize_t caml_round_heap_chunk_size (asize_t request)
{
  asize_t result = request;

  if (result < caml_major_heap_increment){
    result = caml_major_heap_increment;
  }
  result = clip_heap_chunk_size (result);

  if (result < request){
    caml_raise_out_of_memory ();
    return 0; /* not reached */
  }
  return result;
}

void caml_init_major_heap (asize_t heap_size)
{
  caml_stat_heap_size = clip_heap_chunk_size (heap_size);
  caml_stat_top_heap_size = caml_stat_heap_size;
  Assert (caml_stat_heap_size % Page_size == 0);
  caml_heap_start = (char *) caml_alloc_for_heap (caml_stat_heap_size);
  if (caml_heap_start == NULL)
    caml_fatal_error ("Fatal error: not enough memory for the initial heap.\n");
  Chunk_next (caml_heap_start) = NULL;
  caml_stat_heap_chunks = 1;

  if (caml_page_table_add(In_heap, caml_heap_start,
                          caml_heap_start + caml_stat_heap_size) != 0) {
    caml_fatal_error ("Fatal error: not enough memory for the initial page table.\n");
  }

  caml_fl_init_merge ();
  /* caml_make_free_blocks ((value *) caml_heap_start, */
  /*                        Wsize_bsize (caml_stat_heap_size), 1, Caml_white); */
  caml_make_free_blocks_loc ((value *) caml_heap_start,
			     Wsize_bsize (caml_stat_heap_size), 1, Caml_white, PROF_INIT_MAJ_HEAP);

  caml_gc_phase = Phase_idle;
  gray_vals_size = 2048;
  gray_vals = (value *) malloc (gray_vals_size * sizeof (value));
  if (gray_vals == NULL)
    caml_fatal_error ("Fatal error: not enough memory for the gray cache.\n");
  gray_vals_cur = gray_vals;
  gray_vals_end = gray_vals + gray_vals_size;
  heap_is_pure = 1;
  caml_allocated_words = 0;
  caml_extra_heap_resources = 0.0;
}



/* CAGO: Dump heap */
#include "intext.h"
#include "instruct.h"
#include <stdio.h>

#ifndef NATIVE_CODE
#include "stacks.h"
#endif

#include <unistd.h>
#include <string.h>

#define Next(hp) ((hp) + Bhsize_hp (hp))

static FILE *file_oc;

static void store_value(value v)
{
  if(sizeof(value) == 4){
    fputc( (v & 0xff), file_oc);
    fputc( ( (v >> 8) & 0xff), file_oc);
    fputc( ( (v >> 16) & 0xff), file_oc);
    fputc( ( (v >> 24) & 0xff), file_oc);
  } else {
    fputc( (v & 0xff), file_oc);
    fputc( ( (v >> 8) & 0xff), file_oc);
    fputc( ( (v >> 16) & 0xff), file_oc);
    fputc( ( (v >> 24) & 0xff), file_oc);
    fputc( ( (v >> 32) & 0xff), file_oc); 
    fputc( ( (v >> 40) & 0xff), file_oc);
    fputc( ( (v >> 48) & 0xff), file_oc);
    fputc( ( (v >> 56) & 0xff), file_oc);
  }
}

static void check_block (char *hp)
{
  mlsize_t nfields = Wosize_hp (hp);
  mlsize_t i;
  value v = Val_hp (hp);
  value f;
  /* mlsize_t lastbyte; */
  int tag = Tag_hp (hp);
  long locid = Prof_hp(hp);
  
  fputc(1, file_oc); /* 1, a block */
  store_value( v); /* the pointer */
  fputc(tag, file_oc); /* the tag */
  store_value(nfields); /* the size */
  store_value(locid);  /* id which will allow us to find the location ! */
  /* printf(" ## dumping LOCID: %d  -- tag %d\n", locid, tag); */
  
  /* if tag < No_scan_tag only, the contents of the block */
  switch(tag){
    case String_tag:
    case Double_tag:
    case Double_array_tag:
    case Custom_tag: break;
    
    default:
    if(Tag_hp (hp) < No_scan_tag){
      for (i = 0; i < Wosize_hp (hp); i++){
        f = Field (v, i);
#ifndef NATIVE_CODE
        if ((char *) f >= code_area_start && (char *) f < code_area_end) {
#ifdef THREADED_CODE
        if ( *(code_t)f == (opcode_t)(caml_instr_table[RESTART] - caml_instr_base) ){
#else
        if ( *(code_t)f == RESTART ){
#endif
            store_value( (value)code_area_end);
          } else {
            store_value( (value)code_area_start);
          }
        } else
#endif
        {
          store_value( f);
        }
/*
        if (Is_block (f) && Is_in_heap (f)) {
          fprintf( " %x", f);
        }
*/
      }
    }
  }
}

void store_root(value v, value *useless)
{
  if(Is_block(v) && Is_in_heap(v)) store_value(v);
}

extern char *caml_exe_name;

#ifdef NATIVE_CODE
extern char globals_map[];
extern value caml_globals[];
extern value caml_globals_info[];
#endif

static int heap_number = 0;
void really_dump_heap (void)
{
  char *chunk = caml_heap_start, *chunk_end;
  char *cur_hp;
  header_t cur_hd;
  char filename[256];
  sprintf(filename, "heap.dump.%d.%d", getpid(), heap_number++);

  file_oc = fopen(filename, "w");  

  fputc(sizeof(value), file_oc);

{ 
  int size = strlen(caml_exe_name);
  store_value( size);
  fwrite(caml_exe_name, 1, size, file_oc);
}

  while (chunk != NULL){
    chunk_end = chunk + Chunk_size (chunk);

    fputc(0, file_oc); /* 0: a chunk */
    store_value( (value) chunk); /* chunk start */
    store_value( (value) chunk_end); /* chunk end */

    cur_hp = chunk;
    while (cur_hp < chunk_end){
      cur_hd = Hd_hp (cur_hp);
                                           Assert (Next (cur_hp) <= chunk_end);
      switch (Color_hd (cur_hd)){
      case Caml_white:
        if ((Wosize_hd (cur_hd) == 0)
            || (caml_gc_phase == Phase_sweep && cur_hp >= caml_gc_sweep_hp)){
            /* free block */
          }else{
            check_block ( cur_hp);
        }
        break;
      case Caml_gray: case Caml_black:
        check_block ( cur_hp);
        break;
      case Caml_blue:
        /* free block */
        break;
      }
      cur_hp = Next (cur_hp);
    }                                          Assert (cur_hp == chunk_end);
    chunk = Chunk_next (chunk);
  }
  fputc(255, file_oc); /* 255: end of the file */

/* All CLOSURE Codepointers have this value */
  store_value( (value)code_area_start); 
/* All RESTART Codepointers have this value */
  store_value( (value)code_area_end);

#ifdef NATIVE_CODE
  store_value( 0); /* We are in native code */

/* We need to store the globals_map */
{
    value s = globals_map;
    int len = string_length(s);
    store_value( len );
    fwrite(s, 1, len, file_oc);    
}

/* We need to store the caml_globals, and their corresponding pointers */
{
  value* s = caml_globals;
  int pos = 0;
  while(s[pos] != 0) {
    value m = s[pos];
    pos++;

    if(!(Is_block(m))){
      store_value(1); /* Another module */
      store_value(0);
    } else {
      int size = Wosize_val(m);
      int i;
      
      store_value(1); /* Another module */
      store_value(m);
      store_value(size);
      for(i=0; i<size; i++)
        store_value(Field(m,i));
    }
  }
  store_value(0); /* End of the table */
}

/* We need to store the caml_globals, and their corresponding pointers */
{
  value* s = caml_globals_info;
  int pos = 0;
  while(s[pos] != 0) {
    value m = s[pos];
    pos++;

    store_value(1); /* Another module */
    int len = string_length(m);
    store_value( len );
    fwrite(m, 1, len, file_oc);    
  }
  store_value(0); /* End of the table */
}


#else  /* ####### BYTECODE ####### */
  store_value( Val_int(1));
  store_value( caml_global_data); /* global table */
{
  register value * sp;
  int values = 0;
  for (sp = caml_extern_sp; sp < caml_stack_high; sp++) {
    value v = *sp;
    if(Is_block(v) && Is_in_heap(v)) values++;   
  }
  store_value( Val_int(values));  /* number of entries in the stack */
  for (sp = caml_extern_sp; sp < caml_stack_high; sp++) {
    value v = *sp;
    if(Is_block(v) && Is_in_heap(v)) { store_value(1); store_value (v);}
  }
  store_value(0);  /* End */
}
#endif

  caml_do_roots(store_root);

  store_value(Val_int(0)); /* a 0 value at the end */
  fclose(file_oc);
}
