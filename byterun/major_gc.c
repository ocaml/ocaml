#include "config.h"
#include "fail.h"
#include "freelist.h"
#include "gc.h"
#include "gc_ctrl.h"
#include "major_gc.h"
#include "misc.h"
#include "mlvalues.h"
#include "roots.h"

#ifdef __STDC__
#include <limits.h>
#else
#ifdef SIXTYFOUR
#define LONG_MAX 0x7FFFFFFFFFFFFFFF
#else
#define LONG_MAX 0x7FFFFFFF
#endif
#endif

int percent_free;
long major_heap_increment;
char *heap_start, *heap_end;
char *page_table;
asize_t page_table_size;
char *gc_sweep_hp;
int gc_phase;
static value *gray_vals;
value *gray_vals_cur, *gray_vals_end;
static asize_t gray_vals_size;
static int heap_is_pure;   /* The heap is pure if the only gray objects
                              below [markhp] are also in [gray_vals]. */
unsigned long allocated_words;
unsigned long extra_heap_memory;
extern char *fl_merge;  /* Defined in freelist.c. */

static char *markhp, *chunk, *limit;

static void realloc_gray_vals ()
{
  value *new;

  Assert (gray_vals_cur == gray_vals_end);
  if (gray_vals_size < stat_heap_size / 128){
    gc_message ("Growing gray_vals to %ldk\n",
		(long) gray_vals_size * sizeof (value) / 512);
    new = (value *) realloc ((char *) gray_vals,
                             2 * gray_vals_size * sizeof (value));
    if (new == NULL){
      gc_message ("No room for growing gray_vals\n", 0);
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

void darken (v)
     value v;
{
  if (Is_block (v) && Is_in_heap (v) && Is_white_val (v)){
    Hd_val (v) = Grayhd_hd (Hd_val (v));
    *gray_vals_cur++ = v;
    if (gray_vals_cur >= gray_vals_end) realloc_gray_vals ();
  }
}

static void darken_root (p, v)
     value *p;
     value v;
{
  darken (v);
}

static void start_cycle ()
{
  Assert (gray_vals_cur == gray_vals);
  Assert (Is_white_val (global_data));
  darken (global_data);
  scan_local_roots (darken_root);
  gc_phase = Phase_mark;
  markhp = NULL;
}

static void mark_slice (work)
     long work;
{
  value v, child;
  mlsize_t i;

  while (work > 0){
    if (gray_vals_cur > gray_vals){
      v = *--gray_vals_cur;
      Assert (Is_gray_val (v));
      Hd_val (v) = Blackhd_hd (Hd_val (v));
      if (Tag_val (v) < No_scan_tag){
	for (i = Wosize_val (v); i > 0;){
	  --i;
	  child = Field (v, i);
	  darken (child);
	}
      }
      work -= Whsize_val (v);
    }else if (markhp != NULL){
      if (markhp == limit){
	chunk = (((heap_chunk_head *) chunk) [-1]).next;
	if (chunk == NULL){
	  markhp = NULL;
	}else{
	  markhp = chunk;
	  limit = chunk + (((heap_chunk_head *) chunk) [-1]).size;
	}
      }else{
	if (Is_gray_val (Val_hp (markhp))){
	  Assert (gray_vals_cur == gray_vals);
	  *gray_vals_cur++ = Val_hp (markhp);
	}
	markhp += Bhsize_hp (markhp);
      }
    }else if (!heap_is_pure){
      heap_is_pure = 1;
      chunk = heap_start;
      markhp = chunk;
      limit = chunk + (((heap_chunk_head *) chunk) [-1]).size;
    }else{
      /* Marking is done. */
      gc_sweep_hp = heap_start;
      fl_init_merge ();
      gc_phase = Phase_sweep;
      chunk = heap_start;
      gc_sweep_hp = chunk;
      limit = chunk + (((heap_chunk_head *) chunk) [-1]).size;
      work = 0;
    }
  }
}

static void sweep_slice (work)
     long work;
{
  char *hp;
  header_t hd;

  while (work > 0){
    if (gc_sweep_hp < limit){
      hp = gc_sweep_hp;
      hd = Hd_hp (hp);
      work -= Whsize_hd (hd);
      gc_sweep_hp += Bhsize_hd (hd);
      switch (Color_hd (hd)){
      case White:
	if (Tag_hd (hd) == Final_tag){
	  Final_fun (Val_hp (hp)) (Val_hp (hp));
	}
	gc_sweep_hp = fl_merge_block (Bp_hp (hp));
	break;
      case Gray:
	Assert (0);     /* Fall through to Black when not in debug mode. */
      case Black:
	Hd_hp (hp) = Whitehd_hd (hd);
	break;
      case Blue:
	/* Only the blocks of the free-list are blue.  See [freelist.c]. */
	fl_merge = Bp_hp (hp);
	break;
      }
      Assert (gc_sweep_hp <= limit);
    }else{
      chunk = (((heap_chunk_head *) chunk) [-1]).next;
      if (chunk == NULL){
	/* Sweeping is done.  Start the next cycle. */
        ++ stat_major_collections;
	work = 0;
	start_cycle ();
      }else{
	gc_sweep_hp = chunk;
	limit = chunk + (((heap_chunk_head *) chunk) [-1]).size;
      }
    }
  }
}

void major_collection_slice ()
{
  /* Free memory at the start of the GC cycle:
                 FM = stat_heap_size * percent_free / 100 * 2/3
     Proportion of free memory consumed since the previous slice:
                 PH = allocated_words / FM
     Proportion of extra-heap memory consumed since the previous slice:
                 PE = extra_heap_memory / stat_heap_size
     Proportion of total work to do in this slice:
                 P  = PH + PE
     Amount of marking work for the GC cycle:
                 MW = stat_heap_size * (100 - percent_free) / 100
     Amount of sweeping work for the GC cycle:
                 SW = stat_heap_size
     Amount of marking work for this slice:
                 MS = MW * 2 * P
                 MS = 2 * (100 - percent_free)
                      * (allocated_words * 3 / percent_free / 2
		         + 100 * extra_heap_memory)
     Amount of sweeping work for this slice:
                 SS = SW * 2 * P
                 SS = 2 * 100
		      * (allocated_words * 3 / percent_free / 2
		         + 100 * extra_heap_memory)
     This slice will either mark MS words or sweep SS words.
  */

#define Margin 100  /* Make it a little faster to be on the safe side. */

  if (gc_phase == Phase_mark){
    mark_slice (2 * (100 - percent_free)
		* (allocated_words * 3 / percent_free / 2
                   + 100 * extra_heap_memory)
		+ Margin);
    gc_message ("!", 0);
  }else{
    Assert (gc_phase == Phase_sweep);
    sweep_slice (200 * (allocated_words * 3 / percent_free / 2
			+ 100 * extra_heap_memory)
		 + Margin);
    gc_message ("$", 0);
  }
  stat_major_words += allocated_words;
  allocated_words = 0;
  extra_heap_memory = 0;
}

/* The minor heap must be empty when this function is called. */
void finish_major_cycle ()
{
  if (gc_phase == Phase_mark) mark_slice (LONG_MAX);
  Assert (gc_phase == Phase_sweep);
  sweep_slice (LONG_MAX);
  stat_major_words += allocated_words;
  allocated_words = 0;
}

asize_t round_heap_chunk_size (request)
     asize_t request;
{                            Assert (major_heap_increment >= Heap_chunk_min);
  if (request < major_heap_increment){
                              Assert (major_heap_increment % Page_size == 0);
    return major_heap_increment;
  }else if (request <= Heap_chunk_max){
    return ((request + Page_size - 1) >> Page_log) << Page_log;
  }else{
    raise_out_of_memory ();
  }
}

void init_major_heap (heap_size)
     asize_t heap_size;
{
  asize_t i;

  stat_heap_size = round_heap_chunk_size (heap_size);
  Assert (stat_heap_size % Page_size == 0);
  heap_start = aligned_malloc (stat_heap_size + sizeof (heap_chunk_head),
			       sizeof (heap_chunk_head));
  if (heap_start == NULL)
    fatal_error ("Fatal error: not enough memory for the initial heap.\n");
  heap_start += sizeof (heap_chunk_head);
  Assert ((unsigned long) heap_start % Page_size == 0);
  (((heap_chunk_head *) heap_start) [-1]).size = stat_heap_size;
  (((heap_chunk_head *) heap_start) [-1]).next = NULL;
  heap_end = heap_start + stat_heap_size;
  Assert ((unsigned long) heap_end % Page_size == 0);
#ifdef SIXTEEN
  page_table_size = 640L * 1024L / Page_size + 1;
#else
  page_table_size = 4 * stat_heap_size / Page_size;
#endif
  page_table = (char *) malloc (page_table_size);
  if (page_table == NULL){
    fatal_error ("Fatal error: not enough memory for the initial heap.\n");
  }
  for (i = 0; i < page_table_size; i++){
    page_table [i] = Not_in_heap;
  }
  for (i = Page (heap_start); i < Page (heap_end); i++){
    page_table [i] = In_heap;
  }
  Hd_hp (heap_start) = Make_header (Wosize_bhsize (stat_heap_size), 0, Blue);
  fl_init_merge ();
  fl_merge_block (Bp_hp (heap_start));
  /* We start the major GC in the marking phase, just after the roots have been
     darkened. (Since there are no roots, we don't have to darken anything.) */
  gc_phase = Phase_mark;
  gray_vals_size = 2048;
  gray_vals = (value *) malloc (gray_vals_size * sizeof (value));
  gray_vals_cur = gray_vals;
  gray_vals_end = gray_vals + gray_vals_size;
  heap_is_pure = 1;
  allocated_words = 0;
  extra_heap_memory = 0;
}
