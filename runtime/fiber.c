/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*      KC Sivaramakrishnan, Indian Institute of Technology, Madras       */
/*                   Tom Kelly, OCaml Labs Consultancy                    */
/*                Stephen Dolan, University of Cambridge                  */
/*                                                                        */
/*   Copyright 2021 Indian Institute of Technology, Madras                */
/*   Copyright 2021 OCaml Labs Consultancy                                */
/*   Copyright 2019 University of Cambridge                               */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#define CAML_INTERNALS

#include <string.h>
#include <unistd.h>
#include "caml/alloc.h"
#include "caml/codefrag.h"
#include "caml/fail.h"
#include "caml/fiber.h"
#include "caml/gc_ctrl.h"
#include "caml/platform.h"
#include "caml/minor_gc.h"
#include "caml/misc.h"
#include "caml/major_gc.h"
#include "caml/memory.h"
#include "caml/startup_aux.h"
#ifdef NATIVE_CODE
#include "caml/stack.h"
#include "caml/frame_descriptors.h"
#endif
#ifdef USE_MMAP_MAP_STACK
#include <sys/mman.h>
#endif

#ifdef DEBUG
#define fiber_debug_log(...) caml_gc_log(__VA_ARGS__)
#else
#define fiber_debug_log(...)
#endif

static _Atomic int64_t fiber_id = 0;

uintnat caml_get_init_stack_wsize (void)
{
  uintnat default_stack_wsize = Wsize_bsize(Stack_init_bsize);
  uintnat stack_wsize;

  if (default_stack_wsize < caml_max_stack_wsize)
    stack_wsize = default_stack_wsize;
  else
    stack_wsize = caml_max_stack_wsize;

  return stack_wsize;
}

void caml_change_max_stack_size (uintnat new_max_wsize)
{
  struct stack_info *current_stack = Caml_state->current_stack;
  asize_t wsize = Stack_high(current_stack) - (value*)current_stack->sp
                 + Stack_threshold / sizeof (value);

  if (new_max_wsize < wsize) new_max_wsize = wsize;
  if (new_max_wsize != caml_max_stack_wsize){
    caml_gc_log ("Changing stack limit to %"
                 ARCH_INTNAT_PRINTF_FORMAT "uk bytes",
                     new_max_wsize * sizeof (value) / 1024);
  }
  caml_max_stack_wsize = new_max_wsize;
}

#define NUM_STACK_SIZE_CLASSES 5

struct stack_info** caml_alloc_stack_cache (void)
{
  int i;

  struct stack_info** stack_cache =
    (struct stack_info**)caml_stat_alloc_noexc(sizeof(struct stack_info*) *
                                               NUM_STACK_SIZE_CLASSES);
  if (stack_cache == NULL)
    return NULL;

  for(i = 0; i < NUM_STACK_SIZE_CLASSES; i++)
    stack_cache[i] = NULL;

  return stack_cache;
}

Caml_inline struct stack_info* alloc_for_stack (mlsize_t wosize)
{
  size_t len = sizeof(struct stack_info) +
               sizeof(value) * wosize +
               8 /* for alignment to 16-bytes, needed for arm64 */ +
               sizeof(struct stack_handler);
#ifdef USE_MMAP_MAP_STACK
  struct stack_info* si;
  si = mmap(NULL, len, PROT_WRITE | PROT_READ,
             MAP_ANONYMOUS | MAP_PRIVATE | MAP_STACK, -1, 0);
  if (si == MAP_FAILED)
    return NULL;

  si->size = len;
  return si;
#else
  return caml_stat_alloc_noexc(len);
#endif /* USE_MMAP_MAP_STACK */
}

/* Returns the index into the [Caml_state->stack_cache] array if this size is
 * pooled. If unpooled, it is [-1].
 *
 * Stacks may be unpooled if either the stack size is not 2**N multiple of
 * [caml_fiber_wsz] or the stack is bigger than pooled sizes. */
Caml_inline int stack_cache_bucket (mlsize_t wosize) {
  mlsize_t size_bucket_wsz = caml_fiber_wsz;
  int bucket=0;

  while (bucket < NUM_STACK_SIZE_CLASSES) {
    if (wosize == size_bucket_wsz)
      return bucket;
    ++bucket;
    size_bucket_wsz += size_bucket_wsz;
  }

  return -1;
}

static struct stack_info*
alloc_size_class_stack_noexc(mlsize_t wosize, int cache_bucket, value hval,
                             value hexn, value heff, int64_t id)
{
  struct stack_info* stack;
  struct stack_handler* hand;
  struct stack_info **cache = Caml_state->stack_cache;

  CAML_STATIC_ASSERT(sizeof(struct stack_info) % sizeof(value) == 0);
  CAML_STATIC_ASSERT(sizeof(struct stack_handler) % sizeof(value) == 0);

  CAMLassert(cache != NULL);

  if (cache_bucket != -1 &&
      cache[cache_bucket] != NULL) {
    stack = cache[cache_bucket];
    cache[cache_bucket] =
      (struct stack_info*)stack->exception_ptr;
    CAMLassert(stack->cache_bucket == stack_cache_bucket(wosize));
    hand = stack->handler;
  } else {
    /* couldn't get a cached stack, so have to create one */
    stack = alloc_for_stack(wosize);
    if (stack == NULL) {
      return NULL;
    }

    stack->cache_bucket = cache_bucket;

    /* Ensure 16-byte alignment because some architectures require it */
    hand = (struct stack_handler*)
     (((uintnat)stack + sizeof(struct stack_info) + sizeof(value) * wosize + 8)
      & ((uintnat)-1 << 4));
    stack->handler = hand;
  }

  hand->handle_value = hval;
  hand->handle_exn = hexn;
  hand->handle_effect = heff;
  hand->parent = NULL;
  stack->sp = (value*)hand;
  stack->exception_ptr = NULL;
  stack->id = id;
#ifdef DEBUG
  stack->magic = 42;
#endif
  CAMLassert(Stack_high(stack) - Stack_base(stack) == wosize ||
             Stack_high(stack) - Stack_base(stack) == wosize + 1);
  return stack;

}

/* allocate a stack with at least "wosize" usable words of stack */
static struct stack_info*
alloc_stack_noexc(mlsize_t wosize, value hval, value hexn, value heff,
                  int64_t id)
{
  int cache_bucket = stack_cache_bucket (wosize);
  return alloc_size_class_stack_noexc(wosize, cache_bucket, hval, hexn, heff,
                                      id);
}

#ifdef NATIVE_CODE

value caml_alloc_stack (value hval, value hexn, value heff) {
  const int64_t id = atomic_fetch_add(&fiber_id, 1);
  struct stack_info* stack =
    alloc_size_class_stack_noexc(caml_fiber_wsz, 0 /* first bucket */,
                                 hval, hexn, heff, id);

  if (!stack) caml_raise_out_of_memory();

  fiber_debug_log ("Allocate stack=%p of %" ARCH_INTNAT_PRINTF_FORMAT
                     "u words", stack, caml_fiber_wsz);

  return Val_ptr(stack);
}


void caml_get_stack_sp_pc (struct stack_info* stack,
                           char** sp /* out */, uintnat* pc /* out */)
{
  char* p = (char*)stack->sp;

  Pop_frame_pointer(p);
  *pc = *(uintnat*)p; /* ret addr */
  *sp = p + sizeof(value);
}

Caml_inline void scan_stack_frames(
  scanning_action f, scanning_action_flags fflags, void* fdata,
  struct stack_info* stack, value* gc_regs)
{
  char * sp;
  uintnat retaddr;
  value * regs;
  frame_descr * d;
  uintnat h;
  int n, ofs;
  unsigned short * p;
  value *root;
  caml_frame_descrs fds = caml_get_frame_descrs();

  sp = (char*)stack->sp;
  regs = gc_regs;

next_chunk:
  if (sp == (char*)Stack_high(stack)) return;

  Pop_frame_pointer(sp);
  retaddr = *(uintnat*)sp;
  sp += sizeof(value);

  while(1) {
    /* Find the descriptor corresponding to the return address */
    h = Hash_retaddr(retaddr, fds.mask);
    while(1) {
      d = fds.descriptors[h];
      if (d->retaddr == retaddr) break;
      h = (h+1) & fds.mask;
    }
    if (d->frame_size != 0xFFFF) {
      /* Scan the roots in this frame */
      for (p = d->live_ofs, n = d->num_live; n > 0; n--, p++) {
        ofs = *p;
        if (ofs & 1) {
          root = regs + (ofs >> 1);
        } else {
          root = (value *)(sp + ofs);
        }
        f (fdata, *root, root);
      }
      /* Move to next frame */
      sp += (d->frame_size & 0xFFFC);
      retaddr = Saved_return_address(sp);
      /* XXX KC: disabled already scanned optimization. */
    } else {
      /* This marks the top of an ML stack chunk. Move sp to the previous
       * stack chunk.  */
      sp += 3 * sizeof(value); /* trap frame & DWARF pointer */
      regs = *(value**)sp;     /* update gc_regs */
      sp += 1 * sizeof(value); /* gc_regs */
      goto next_chunk;
    }
  }
}

void caml_scan_stack(
  scanning_action f, scanning_action_flags fflags, void* fdata,
  struct stack_info* stack, value* gc_regs)
{
  while (stack != NULL) {
    scan_stack_frames(f, fflags, fdata, stack, gc_regs);

    f(fdata, Stack_handle_value(stack), &Stack_handle_value(stack));
    f(fdata, Stack_handle_exception(stack), &Stack_handle_exception(stack));
    f(fdata, Stack_handle_effect(stack), &Stack_handle_effect(stack));

    stack = Stack_parent(stack);
  }
}

void caml_maybe_expand_stack (void)
{
  struct stack_info* stk = Caml_state->current_stack;
  uintnat stack_available =
    (value*)stk->sp - Stack_base(stk);
  uintnat stack_needed =
    Stack_threshold / sizeof(value)
    + 8 /* for words pushed by caml_start_program */;

  if (stack_available < stack_needed)
    if (!caml_try_realloc_stack (stack_needed))
      caml_raise_stack_overflow();

  if (Caml_state->gc_regs_buckets == NULL) {
    /* ensure there is at least one gc_regs bucket available before
       running any OCaml code */
    value* bucket = caml_stat_alloc(sizeof(value) * Wosize_gc_regs);
    bucket[0] = 0; /* no next bucket */
    Caml_state->gc_regs_buckets = bucket;
  }
}

#else /* End NATIVE_CODE, begin BYTE_CODE */

value caml_global_data;

CAMLprim value caml_alloc_stack(value hval, value hexn, value heff)
{
  value* sp;
  const int64_t id = atomic_fetch_add(&fiber_id, 1);
  struct stack_info* stack =
    alloc_size_class_stack_noexc(caml_fiber_wsz, 0 /* first bucket */,
                                 hval, hexn, heff, id);

  if (!stack) caml_raise_out_of_memory();

  sp = Stack_high(stack);
  sp -= 1;
  sp[0] = Val_long(1);

  stack->sp = sp;

  return Val_ptr(stack);
}

CAMLprim value caml_ensure_stack_capacity(value required_space)
{
  asize_t req = Long_val(required_space);
  if (Caml_state->current_stack->sp - req <
      Stack_base(Caml_state->current_stack))
    if (!caml_try_realloc_stack(req))
      caml_raise_stack_overflow();
  return Val_unit;
}

/*
  Root scanning.

  Used by the GC to find roots on the stacks of running or runnable fibers.
*/

/* Code pointers are stored on the bytecode stack as naked pointers.
   We must avoid passing them to the scanning action,
   unless we know that it is a no-op outside young values
   (so it will safely ignore code pointers). */
 Caml_inline int is_scannable(scanning_action_flags flags, value v) {
  return
      (flags & SCANNING_ONLY_YOUNG_VALUES)
      || (Is_block(v) && caml_find_code_fragment_by_pc((char *) v) == NULL);
}

void caml_scan_stack(
  scanning_action f, scanning_action_flags fflags, void* fdata,
  struct stack_info* stack, value* v_gc_regs)
{
  value *low, *high, *sp;

  while (stack != NULL) {
    CAMLassert(stack->magic == 42);

    high = Stack_high(stack);
    low = stack->sp;
    for (sp = low; sp < high; sp++) {
      value v = *sp;
      if (is_scannable(fflags, v)) {
        f(fdata, v, sp);
      }
    }

    if (is_scannable(fflags, Stack_handle_value(stack)))
      f(fdata, Stack_handle_value(stack), &Stack_handle_value(stack));
    if (is_scannable(fflags, Stack_handle_exception(stack)))
      f(fdata, Stack_handle_exception(stack), &Stack_handle_exception(stack));
    if (is_scannable(fflags, Stack_handle_effect(stack)))
      f(fdata, Stack_handle_effect(stack), &Stack_handle_effect(stack));

    stack = Stack_parent(stack);
  }
}

#endif /* end BYTE_CODE */

/*
  Stack management.

  Used by the interpreter to allocate stack space.
*/

#ifdef NATIVE_CODE
/* Update absolute exception pointers for new stack*/
static void
rewrite_exception_stack(struct stack_info *old_stack,
                        value** exn_ptr, struct stack_info *new_stack)
{
  fiber_debug_log("Old [%p, %p]", Stack_base(old_stack), Stack_high(old_stack));
  fiber_debug_log("New [%p, %p]", Stack_base(new_stack), Stack_high(new_stack));
  if(exn_ptr) {
    fiber_debug_log ("*exn_ptr=%p", *exn_ptr);

    while (Stack_base(old_stack) < *exn_ptr &&
           *exn_ptr <= Stack_high(old_stack)) {
#ifdef DEBUG
      value* old_val = *exn_ptr;
#endif
      *exn_ptr = Stack_high(new_stack) - (Stack_high(old_stack) - *exn_ptr);

      fiber_debug_log ("Rewriting %p to %p", old_val, *exn_ptr);

      CAMLassert(Stack_base(new_stack) < *exn_ptr);
      CAMLassert((value*)*exn_ptr <= Stack_high(new_stack));

      exn_ptr = (value**)*exn_ptr;
    }
    fiber_debug_log ("finished with *exn_ptr=%p", *exn_ptr);
  } else {
    fiber_debug_log ("exn_ptr is null");
  }
}
#endif

int caml_try_realloc_stack(asize_t required_space)
{
  struct stack_info *old_stack, *new_stack;
  asize_t wsize;
  int stack_used;
  CAMLnoalloc;

  old_stack = Caml_state->current_stack;
  stack_used = Stack_high(old_stack) - (value*)old_stack->sp;
  wsize = Stack_high(old_stack) - Stack_base(old_stack);
  do {
    if (wsize >= caml_max_stack_wsize) return 0;
    wsize *= 2;
  } while (wsize < stack_used + required_space);

  if (wsize > 4096 / sizeof(value)) {
    caml_gc_log ("Growing stack to %"
                 ARCH_INTNAT_PRINTF_FORMAT "uk bytes",
                 (uintnat) wsize * sizeof(value) / 1024);
  } else {
    caml_gc_log ("Growing stack to %"
                 ARCH_INTNAT_PRINTF_FORMAT "u bytes",
                 (uintnat) wsize * sizeof(value));
  }

  new_stack = alloc_stack_noexc(wsize,
                                Stack_handle_value(old_stack),
                                Stack_handle_exception(old_stack),
                                Stack_handle_effect(old_stack),
                                old_stack->id);
  if (!new_stack) return 0;
  memcpy(Stack_high(new_stack) - stack_used,
         Stack_high(old_stack) - stack_used,
         stack_used * sizeof(value));
  new_stack->sp = Stack_high(new_stack) - stack_used;
  Stack_parent(new_stack) = Stack_parent(old_stack);
#ifdef NATIVE_CODE
  rewrite_exception_stack(old_stack, (value**)&Caml_state->exn_handler,
                          new_stack);
#endif

  /* Update stack pointers in Caml_state->c_stack. It is possible to have
   * multiple c_stack_links to point to the same stack since callbacks are run
   * on existing stacks. */
  {
    struct c_stack_link* link;
    for (link = Caml_state->c_stack; link; link = link->prev) {
      if (link->stack == old_stack) {
        link->stack = new_stack;
        link->sp = (void*)((char*)Stack_high(new_stack) -
                           ((char*)Stack_high(old_stack) - (char*)link->sp));
      }
    }
  }

  caml_free_stack(old_stack);
  Caml_state->current_stack = new_stack;
  return 1;
}

struct stack_info* caml_alloc_main_stack (uintnat init_wsize)
{
  const int64_t id = atomic_fetch_add(&fiber_id, 1);
  struct stack_info* stk =
    alloc_stack_noexc(init_wsize, Val_unit, Val_unit, Val_unit, id);
  return stk;
}

void caml_free_stack (struct stack_info* stack)
{
  CAMLnoalloc;
  struct stack_info** cache = Caml_state->stack_cache;

  CAMLassert(stack->magic == 42);
  CAMLassert(cache != NULL);
  if (stack->cache_bucket != -1) {
    stack->exception_ptr =
      (void*)(cache[stack->cache_bucket]);
    cache[stack->cache_bucket] = stack;
#ifdef DEBUG
    memset(Stack_base(stack), 0x42,
           (Stack_high(stack)-Stack_base(stack))*sizeof(value));
#endif
  } else {
#ifdef DEBUG
    memset(stack, 0x42, (char*)stack->handler - (char*)stack);
#endif
#ifdef USE_MMAP_MAP_STACK
    munmap(stack, stack->size);
#else
    caml_stat_free(stack);
#endif
  }
}

CAMLprim value caml_continuation_use_noexc (value cont)
{
  value v;
  value null_stk = Val_ptr(NULL);
  CAMLnoalloc;

  fiber_debug_log("cont: is_block(%d) tag_val(%ul) is_young(%d)",
                  Is_block(cont), Tag_val(cont), Is_young(cont));
  CAMLassert(Is_block(cont) && Tag_val(cont) == Cont_tag);

  /* this forms a barrier between execution and any other domains
     that might be marking this continuation */
  if (!Is_young(cont) ) caml_darken_cont(cont);

  /* at this stage the stack is assured to be marked */
  v = Field(cont, 0);

  if (caml_domain_alone()) {
    Field(cont, 0) = null_stk;
    return v;
  }

  if (atomic_compare_exchange_strong(Op_atomic_val(cont), &v, null_stk)) {
    return v;
  } else {
    return null_stk;
  }
}

CAMLprim value caml_continuation_use (value cont)
{
  value v = caml_continuation_use_noexc(cont);
  if (v == Val_ptr(NULL))
    caml_raise_continuation_already_taken();
  return v;
}

CAMLprim value caml_continuation_use_and_update_handler_noexc
  (value cont, value hval, value hexn, value heff)
{
  value stack;
  struct stack_info* stk;

  stack = caml_continuation_use_noexc (cont);
  stk = Ptr_val(stack);
  if (stk == NULL) {
    /* The continuation has already been taken */
    return stack;
  }
  while (Stack_parent(stk) != NULL) stk = Stack_parent(stk);
  Stack_handle_value(stk) = hval;
  Stack_handle_exception(stk) = hexn;
  Stack_handle_effect(stk) = heff;
  return stack;
}

void caml_continuation_replace(value cont, struct stack_info* stk)
{
  value n = Val_ptr(NULL);
  int b = atomic_compare_exchange_strong(Op_atomic_val(cont), &n, Val_ptr(stk));
  CAMLassert(b);
  (void)b; /* squash unused warning */
}

CAMLprim value caml_drop_continuation (value cont)
{
  struct stack_info* stk = Ptr_val(caml_continuation_use(cont));
  caml_free_stack(stk);
  return Val_unit;
}
