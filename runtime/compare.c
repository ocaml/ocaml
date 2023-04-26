/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 1996 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#define CAML_INTERNALS

#include <string.h>
#include <stdlib.h>
#include "caml/custom.h"
#include "caml/fail.h"
#include "caml/memory.h"
#include "caml/misc.h"
#include "caml/mlvalues.h"

/* Structural comparison on trees. */

struct compare_item { value v1, v2, offset, size; };

#define COMPARE_STACK_INIT_SIZE 8
#define COMPARE_STACK_MIN_ALLOC_SIZE 32
#define COMPARE_STACK_MAX_SIZE (1024*1024)

struct compare_stack {
  struct compare_item init_stack[COMPARE_STACK_INIT_SIZE];
  struct compare_item* stack;
  struct compare_item* limit;
};

#define COMPARE_POLL_PERIOD 1024

/* Free the compare stack if needed */
static void compare_free_stack(struct compare_stack* stk)
{
  if (stk->stack != stk->init_stack) {
    caml_stat_free(stk->stack);
    stk->stack = NULL;
  }
}

/* Same, then raise Out_of_memory */
CAMLnoreturn_start
static void compare_stack_overflow(struct compare_stack* stk)
CAMLnoreturn_end;

static void compare_stack_overflow(struct compare_stack* stk)
{
  caml_gc_message (0x04, "Stack overflow in structural comparison\n");
  compare_free_stack(stk);
  caml_raise_out_of_memory();
}

/* Grow the compare stack */
static struct compare_item * compare_resize_stack(struct compare_stack* stk,
                                                  struct compare_item * sp)
{
  asize_t newsize;
  asize_t sp_offset = sp - stk->stack;
  struct compare_item * newstack;

  if (stk->stack == stk->init_stack) {
    newsize = COMPARE_STACK_MIN_ALLOC_SIZE;
    newstack = caml_stat_alloc_noexc(sizeof(struct compare_item) * newsize);
    if (newstack == NULL) compare_stack_overflow(stk);
    memcpy(newstack, stk->init_stack,
           sizeof(struct compare_item) * COMPARE_STACK_INIT_SIZE);
  } else {
    newsize = 2 * (stk->limit - stk->stack);
    if (newsize >= COMPARE_STACK_MAX_SIZE) compare_stack_overflow(stk);
    newstack = caml_stat_resize_noexc(stk->stack,
                                      sizeof(struct compare_item) * newsize);
    if (newstack == NULL) compare_stack_overflow(stk);
  }
  stk->stack = newstack;
  stk->limit = newstack + newsize;
  return newstack + sp_offset;
}

static intnat do_compare_val(struct compare_stack* stk,
                             value v1, value v2, int total);

static intnat compare_val(value v1, value v2, int total)
{
  struct compare_stack stk;
  intnat res;
  stk.stack = stk.init_stack;
  stk.limit = stk.stack + COMPARE_STACK_INIT_SIZE;
  res = do_compare_val(&stk, v1, v2, total);
  compare_free_stack(&stk);
  return res;
}

static void run_pending_actions(struct compare_stack* stk,
                                struct compare_item* sp)
{
  value exn;
  value* roots_start = (value*)(stk->stack);
  size_t roots_length =
    (sp - stk->stack)
    * sizeof(struct compare_item) / sizeof(value);
  Begin_roots_block(roots_start, roots_length);
  exn = caml_do_pending_actions_exn();
  End_roots();
  if (Is_exception_result(exn)) {
    exn = Extract_exception(exn);
    compare_free_stack(stk);
    caml_raise(exn);
  }
}

/* Structural comparison */


#define LESS -1
#define EQUAL 0
#define GREATER 1
#define UNORDERED ((intnat)1 << (8 * sizeof(value) - 1))

/* The return value of compare_val is as follows:
      > 0                 v1 is greater than v2
      0                   v1 is equal to v2
      < 0 and > UNORDERED v1 is less than v2
      UNORDERED           v1 and v2 cannot be compared */

static intnat do_compare_val(struct compare_stack* stk,
                             value v1, value v2, int total)
{
  struct compare_item * sp;
  tag_t t1, t2;
  int poll_timer;

  sp = stk->stack;
  while (1) {
    poll_timer = COMPARE_POLL_PERIOD;
    while (--poll_timer > 0) {
      if (v1 == v2 && total) goto next_item;
      if (Is_long(v1)) {
        if (v1 == v2) goto next_item;
        if (Is_long(v2))
          return Long_val(v1) - Long_val(v2);
        /* Subtraction above cannot overflow and cannot result in UNORDERED */
        switch (Tag_val(v2)) {
          case Forward_tag:
            v2 = Forward_val(v2);
            continue;
          case Custom_tag: {
            int res;
            int (*compare)(value v1, value v2) =
              Custom_ops_val(v2)->compare_ext;
            if (compare == NULL) break;  /* for backward compatibility */
            Caml_state->compare_unordered = 0;
            res = compare(v1, v2);
            if (Caml_state->compare_unordered && !total) return UNORDERED;
            if (res != 0) return res;
            goto next_item;
          }
          default: /*fallthrough*/;
          }

        return LESS;                /* v1 long < v2 block */
      }
      if (Is_long(v2)) {
        switch (Tag_val(v1)) {
          case Forward_tag:
            v1 = Forward_val(v1);
            continue;
          case Custom_tag: {
            int res;
            int (*compare)(value v1, value v2) =
              Custom_ops_val(v1)->compare_ext;
            if (compare == NULL) break;  /* for backward compatibility */
            Caml_state->compare_unordered = 0;
            res = compare(v1, v2);
            if (Caml_state->compare_unordered && !total) return UNORDERED;
            if (res != 0) return res;
            goto next_item;
          }
          default: /*fallthrough*/;
          }
        return GREATER;            /* v1 block > v2 long */
      }
      t1 = Tag_val(v1);
      t2 = Tag_val(v2);
      if (t1 != t2) {
          /* Besides long/block comparisons, the only forms of
             heterogeneous comparisons we support are:
             - Forward_tag pointers, which may point to values of any type, and
             - comparing Infix_tag and Closure_tag functions (#9521).

             Other heterogeneous cases may still happen due to
             existential types, and we just compare the tags.
          */
          if (t1 == Forward_tag) { v1 = Forward_val (v1); continue; }
          if (t2 == Forward_tag) { v2 = Forward_val (v2); continue; }
          if (t1 == Infix_tag) t1 = Closure_tag;
          if (t2 == Infix_tag) t2 = Closure_tag;
          if (t1 != t2)
              return (intnat)t1 - (intnat)t2;
      }
      switch(t1) {
      case Forward_tag: {
          v1 = Forward_val (v1);
          v2 = Forward_val (v2);
          continue;
      }
      case String_tag: {
        mlsize_t len1, len2;
        int res;
        if (v1 == v2) break;
        len1 = caml_string_length(v1);
        len2 = caml_string_length(v2);
        res = memcmp(String_val(v1), String_val(v2),
                     len1 <= len2 ? len1 : len2);
        if (res < 0) return LESS;
        if (res > 0) return GREATER;
        if (len1 != len2) return len1 - len2;
        break;
      }
      case Double_tag: {
        double d1 = Double_val(v1);
        double d2 = Double_val(v2);
        if (d1 < d2) return LESS;
        if (d1 > d2) return GREATER;
        if (d1 != d2) {
          if (! total) return UNORDERED;
          /* One or both of d1 and d2 is NaN.  Order according to the
             convention NaN = NaN and NaN < f for all other floats f. */
          if (d1 == d1) return GREATER; /* d1 is not NaN, d2 is NaN */
          if (d2 == d2) return LESS;    /* d2 is not NaN, d1 is NaN */
          /* d1 and d2 are both NaN, thus equal: continue comparison */
        }
        break;
      }
      case Double_array_tag: {
        mlsize_t sz1 = Wosize_val(v1) / Double_wosize;
        mlsize_t sz2 = Wosize_val(v2) / Double_wosize;
        mlsize_t i;
        if (sz1 != sz2) return sz1 - sz2;
        for (i = 0; i < sz1; i++) {
          double d1 = Double_flat_field(v1, i);
          double d2 = Double_flat_field(v2, i);
          if (d1 < d2) return LESS;
          if (d1 > d2) return GREATER;
          if (d1 != d2) {
            if (! total) return UNORDERED;
            /* See comment for Double_tag case */
            if (d1 == d1) return GREATER;
            if (d2 == d2) return LESS;
          }
        }
        break;
      }
      case Abstract_tag:
        compare_free_stack(stk);
        caml_invalid_argument("compare: abstract value");
      case Closure_tag:
      case Infix_tag:
        compare_free_stack(stk);
        caml_invalid_argument("compare: functional value");
      case Cont_tag:
        compare_free_stack(stk);
        caml_invalid_argument("compare: continuation value");
      case Object_tag: {
        intnat oid1 = Oid_val(v1);
        intnat oid2 = Oid_val(v2);
        if (oid1 != oid2) return oid1 - oid2;
        break;
      }
      case Custom_tag: {
        int res;
        int (*compare)(value v1, value v2) = Custom_ops_val(v1)->compare;
        /* Hardening against comparisons between different types */
        if (compare != Custom_ops_val(v2)->compare) {
          return strcmp(Custom_ops_val(v1)->identifier,
                        Custom_ops_val(v2)->identifier) < 0
                 ? LESS : GREATER;
        }
        if (compare == NULL) {
          compare_free_stack(stk);
          caml_invalid_argument("compare: abstract value");
        }
        Caml_state->compare_unordered = 0;
        res = compare(v1, v2);
        if (Caml_state->compare_unordered && !total) return UNORDERED;
        if (res != 0) return res;
        break;
      }
      default: {
        mlsize_t sz1 = Wosize_val(v1);
        mlsize_t sz2 = Wosize_val(v2);
        /* Compare sizes first for speed */
        if (sz1 != sz2) return sz1 - sz2;
        if (sz1 == 0) break;
        /* Remember that we still have to compare fields 1 ... sz - 1. */
        if (sz1 > 1) {
          if (sp >= stk->limit) sp = compare_resize_stack(stk, sp);
          struct compare_item* next = sp++;
          next->v1 = v1;
          next->v2 = v2;
          next->size = Val_long(sz1);
          next->offset = Val_long(1);
        }
        /* Continue comparison with first field */
        v1 = Field(v1, 0);
        v2 = Field(v2, 0);
        continue;
      }
      }
    next_item:
      /* Pop one more item to compare, if any */
      if (sp == stk->stack) return EQUAL; /* we're done */

      struct compare_item* last = sp-1;
      v1 = Field(last->v1, Long_val(last->offset));
      v2 = Field(last->v2, Long_val(last->offset));
      last->offset += 2;/* Long_val(last->offset) += 1 */
      if (last->offset == last->size) sp--;
    }
    /* Poll for actions */
    if (caml_check_pending_actions()) {
      /* Preserve v1, v2 if a GC occurs.  Registering v1 and v2 directly
         as roots would prevent them from being allocated to registers. */
      value root_v1 = v1, root_v2 = v2;
      Begin_roots2(root_v1, root_v2);
      run_pending_actions(stk, sp);
      v1 = root_v1; v2 = root_v2;
      End_roots();
    }
    /* Resume comparison after resetting poll_timer */
  }
}

CAMLprim value caml_compare(value v1, value v2)
{
  intnat res = compare_val(v1, v2, 1);
  if (res < 0)
    return Val_int(LESS);
  else if (res > 0)
    return Val_int(GREATER);
  else
    return Val_int(EQUAL);
}

CAMLprim value caml_equal(value v1, value v2)
{
  intnat res = compare_val(v1, v2, 0);
  return Val_int(res == 0);
}

CAMLprim value caml_notequal(value v1, value v2)
{
  intnat res = compare_val(v1, v2, 0);
  return Val_int(res != 0);
}

CAMLprim value caml_lessthan(value v1, value v2)
{
  intnat res = compare_val(v1, v2, 0);
  return Val_int(res < 0 && res != UNORDERED);
}

CAMLprim value caml_lessequal(value v1, value v2)
{
  intnat res = compare_val(v1, v2, 0);
  return Val_int(res <= 0 && res != UNORDERED);
}

CAMLprim value caml_greaterthan(value v1, value v2)
{
  intnat res = compare_val(v1, v2, 0);
  return Val_int(res > 0);
}

CAMLprim value caml_greaterequal(value v1, value v2)
{
  intnat res = compare_val(v1, v2, 0);
  return Val_int(res >= 0);
}
