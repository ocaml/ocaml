#include <string.h>
#include "config.h"
#include "fail.h"
#include "gc.h"
#include "gc_ctrl.h"
#include "major_gc.h"
#include "memory.h"
#include "minor_gc.h"
#include "misc.h"
#include "mlvalues.h"
#include "roots.h"

asize_t minor_heap_size;
char *young_start = NULL, *young_end, *young_ptr = NULL;
static value **ref_table = NULL, **ref_table_end, **ref_table_threshold;
value **ref_table_ptr = NULL, **ref_table_limit;
static asize_t ref_table_size, ref_table_reserve;

void set_minor_heap_size (size)
    asize_t size;
{
  char *new_heap;
  value **new_table;

  Assert (size >= Minor_heap_min);
  Assert (size <= Minor_heap_max);
  Assert (size % sizeof (value) == 0);
  if (young_ptr != young_start) minor_collection ();
                                           Assert (young_ptr == young_start);
  new_heap = (char *) stat_alloc (size);
  if (young_start != NULL){
    stat_free ((char *) young_start);
  }
  young_start = new_heap;
  young_end = new_heap + size;
  young_ptr = young_start;
  minor_heap_size = size;

  ref_table_size = minor_heap_size / sizeof (value) / 8;
  ref_table_reserve = 256;
  new_table = (value **) stat_alloc ((ref_table_size + ref_table_reserve)
				     * sizeof (value *));
  if (ref_table != NULL) stat_free ((char *) ref_table);
  ref_table = new_table;
  ref_table_ptr = ref_table;
  ref_table_threshold = ref_table + ref_table_size;
  ref_table_limit = ref_table_threshold;
  ref_table_end = ref_table + ref_table_size + ref_table_reserve;
}

static void oldify (p, v)
     value *p;
     value v;
{
  value result;
  mlsize_t i;

 tail_call:
  if (Is_block (v) && Is_young (v)){
    Assert (Hp_val (v) < young_ptr);
    if (Is_blue_val (v)){    /* Already forwarded ? */
      *p = Field (v, 0);     /* Then the forward pointer is the first field. */
    }else if (Tag_val (v) >= No_scan_tag){
      result = alloc_shr (Wosize_val (v), Tag_val (v));
      bcopy (Bp_val (v), Bp_val (result), Bosize_val (v));
      Hd_val (v) = Bluehd_hd (Hd_val (v));    /* Put the forward flag. */
      Field (v, 0) = result;                  /* And the forward pointer. */
      *p = result;
    }else{
      /* We can do recursive calls before all the fields are filled, because
         we will not be calling the major GC. */
      value field0 = Field (v, 0);
      mlsize_t sz = Wosize_val (v);

      result = alloc_shr (sz, Tag_val (v));
      *p = result;
      Hd_val (v) = Bluehd_hd (Hd_val (v));    /* Put the forward flag. */
      Field (v, 0) = result;                  /* And the forward pointer. */
      if (sz == 1){
        p = &Field (result, 0);
        v = field0;
        goto tail_call;
      }else{
        oldify (&Field (result, 0), field0);
        for (i = 1; i < sz - 1; i++){
          oldify (&Field (result, i), Field (v, i));
        }
        p = &Field (result, i);
        v = Field (v, i);
        goto tail_call;
      }
    }
  }else{
    *p = v;
  }
}

void minor_collection ()
{
  value **r;
  struct longjmp_buffer raise_buf;
  struct longjmp_buffer *old_external_raise;
  long prev_alloc_words = allocated_words;

  if (setjmp(raise_buf.buf)) {
    fatal_error ("Fatal error: out of memory.\n");
  }
  old_external_raise = external_raise;
  external_raise = &raise_buf;

  gc_message ("<", 0);
  scan_local_roots (oldify);
  for (r = ref_table; r < ref_table_ptr; r++) oldify (*r, **r);
  stat_minor_words += Wsize_bsize (young_ptr - young_start);
  young_ptr = young_start;
  ref_table_ptr = ref_table;
  ref_table_limit = ref_table_threshold;
  gc_message (">", 0);

  external_raise = old_external_raise;

  stat_promoted_words += allocated_words - prev_alloc_words;
  ++ stat_minor_collections;
  major_collection_slice ();
  force_minor_flag = 0;
}

void realloc_ref_table ()
{                                 Assert (ref_table_ptr == ref_table_limit);
                                  Assert (ref_table_limit <= ref_table_end);
                            Assert (ref_table_limit >= ref_table_threshold);

  if (ref_table_limit == ref_table_threshold){
    gc_message ("ref_table threshold crossed\n", 0);
    ref_table_limit = ref_table_end;
    force_minor_gc ();
  }else{                                       /* This will never happen. */
    asize_t sz;
    asize_t cur_ptr = ref_table_ptr - ref_table;
                                                  Assert (force_minor_flag);
                                                   Assert (something_to_do);
    ref_table_reserve += 1024;
    sz = (ref_table_size + ref_table_reserve) * sizeof (value *);
    gc_message ("Growing ref_table to %ldk\n", (long) sz / 1024);
#ifdef MAX_MALLOC_SIZE
    if (sz > MAX_MALLOC_SIZE) ref_table = NULL;
    else
#endif
    ref_table = (value **) realloc ((char *) ref_table, sz);
    if (ref_table == NULL) fatal_error ("Fatal error: ref_table overflow\n");
    ref_table_end = ref_table + ref_table_size + ref_table_reserve;
    ref_table_threshold = ref_table + ref_table_size;
    ref_table_ptr = ref_table + cur_ptr;
    ref_table_limit = ref_table_end;
  }
}
