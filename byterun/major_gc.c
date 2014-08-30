#include <stdlib.h>

#include "mlvalues.h"
#include "memory.h"
#include "fail.h"
#include "shared_heap.h"
#include "memory.h"
#include "roots.h"
#include "globroots.h"
#include "domain.h"
#include "fiber.h"

intnat caml_major_collection_slice (intnat work) {
  if ((rand() % 10) < 2)  caml_trigger_stw_gc();
  /* caml_finish_major_cycle(); */
  return 100;
}

#define MARK_STACK_SIZE (1 << 20)
__thread value* caml_mark_stack;
__thread int caml_mark_stack_count;

static __thread uintnat stat_blocks_marked = 0;

void caml_init_major_gc() {
  caml_mark_stack = caml_stat_alloc(MARK_STACK_SIZE * sizeof(value));
  caml_mark_stack_count = 0;
}

static void mark_stack_push(value v) {
  Assert(Is_block(v));
  if (caml_mark_stack_count >= MARK_STACK_SIZE)
    caml_failwith("mark stack overflow");
  caml_mark_stack[caml_mark_stack_count++] = v;
}

static int mark_stack_pop(value* ret) {
  if (caml_mark_stack_count == 0) 
    return 0;
  *ret = caml_mark_stack[--caml_mark_stack_count];
  return 1;
}

#define Is_markable(v) (Is_block(v) && !Is_minor(v))

static value mark_normalise(value v) {
  Assert(Is_markable(v));
  if (Tag_val(v) == Forward_tag) {
    /* FIXME: short-circuiting lazy values is a useful optimisation */
  } else if (Tag_val(v) == Infix_tag) {
    v -= Infix_offset_val(v);
  }
  return v;
}

static void mark(value initial) {
  value next = initial;
  int found_next = 1;
  while (found_next) {
    value v = next;
    header_t hd_v;
    found_next = 0;

    Assert(Is_markable(v));
    Assert(v == mark_normalise(v));

    stat_blocks_marked++;
    /* mark the current object */
    hd_v = Hd_val(v);
    if (Tag_hd (hd_v) == Stack_tag) {
      caml_scan_stack(&caml_darken, v);
    } else if (Tag_hd (hd_v) < No_scan_tag) {
      int i;
      for (i = 0; i < Wosize_hd(hd_v); i++) {
        value child = Field(v, i);
        if (Is_markable(child)) {
          child = mark_normalise(child);
          if (caml_mark_object(child)) {
            if (!found_next) {
              next = child;
              found_next = 1;
            } else {
              mark_stack_push(child);
            }
          }
        }
      }
    }
    
    /* if we haven't found any markable children, pop an object to mark */
    if (!found_next) {
      found_next = mark_stack_pop(&next);
    }
  }
}

void caml_mark_root(value p, value* ptr) {
  if (!p) return;

  caml_darken(p, ptr);
}

void caml_darken(value v, value* ignored) {
  /* Assert (Is_markable(v)); */
  if (!Is_markable (v)) return; /* foreign stack, at least */

  v = mark_normalise(v);
  if (caml_mark_object(v)) mark_stack_push(v);
}

void caml_finish_marking () {
  struct caml_sampled_roots roots;

  caml_save_stack_gc();
  
  caml_sample_local_roots(&roots);
  caml_do_local_roots(&caml_mark_root, &roots);

  caml_scan_global_roots(&caml_mark_root);
  caml_do_foreign_roots(&caml_mark_root);

  caml_empty_mark_stack();
  caml_restore_stack_gc();
}

void caml_empty_mark_stack () {
  value v;

  while (mark_stack_pop(&v)) mark(v);

  if (stat_blocks_marked) 
    caml_gc_log("Finished marking major heap. Marked %u blocks", (unsigned)stat_blocks_marked);
  stat_blocks_marked = 0;
}
