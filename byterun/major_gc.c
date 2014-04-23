#include <stdlib.h>

#include "mlvalues.h"
#include "memory.h"
#include "fail.h"
#include "shared_heap.h"
#include "memory.h"
#include "roots.h"
#include "domain.h"

intnat caml_major_collection_slice (intnat work) {
  if ((rand() % 10) < 2)  caml_trigger_stw_gc();
  /* caml_finish_major_cycle(); */
  return 100;
}

#define MARK_STACK_SIZE (1 << 16)
static __thread value* mark_stack;
static __thread int mark_stack_count;

static __thread uintnat stat_blocks_marked = 0;

void caml_init_major_gc() {
  mark_stack = caml_stat_alloc(MARK_STACK_SIZE * sizeof(value));
  mark_stack_count = 0;
}

static void mark_stack_push(value v) {
  Assert(Is_block(v));
  if (mark_stack_count >= MARK_STACK_SIZE)
    caml_failwith("mark stack overflow");
  mark_stack[mark_stack_count++] = v;
}

static int mark_stack_pop(value* ret) {
  if (mark_stack_count == 0) 
    return 0;
  *ret = mark_stack[--mark_stack_count];
  return 1;
}

void mark(value initial) {
  Assert(Is_block(initial));
  value next = initial;
  int found_next = 1;
  while (found_next) {
    value v = next;
    header_t hd_v = Hd_val(v);
    found_next = 0;

    stat_blocks_marked++;
    /* mark the current object */
    if (Tag_hd (hd_v) < No_scan_tag) {
      int i;
      for (i = 0; i < Wosize_hd(hd_v); i++) {
        value child = Field(v, i);
        if (Is_block(child) && !Is_young(child)) {
          header_t hd_child = Hd_val(child);
          if (Tag_hd (hd_child) == Forward_tag) {
            /* FIXME: short-circuiting lazy values is a useful optimisation */
          } else if (Tag_hd (hd_child) == Infix_tag) {
            child -= Infix_offset_val(child);
            hd_child = Hd_val(child);
          }
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

static void mark_root(value p, value* ptr) {
  if (!p) return;
  /* fixme infix, etc */
  if (Is_block(p) && !Is_young(p)) {
    if (Tag_hd (Hd_val(p)) == Infix_tag) p -= Infix_offset_val(p);
    if (caml_mark_object(p)) mark(p);
  }
}

void caml_finish_marking () {
  caml_do_roots(&mark_root);
  int i;
  for (i = 0 ; i < 256; i ++) caml_mark_object(caml_atom(i));

  caml_gc_log("Finished marking major heap. Marked %u blocks", (unsigned)stat_blocks_marked);
  stat_blocks_marked = 0;
}

