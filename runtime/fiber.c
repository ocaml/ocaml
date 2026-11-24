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

#include "caml/config.h"
#include <string.h>
#ifndef _WIN32
#include <unistd.h>
#endif
#include <assert.h>
#include "caml/alloc.h"
#include "caml/callback.h"
#include "caml/codefrag.h"
#include "caml/fail.h"
#include "caml/fiber.h"
#include "caml/gc_ctrl.h"
#include "caml/platform.h"
#include "caml/minor_gc.h"
#include "caml/signals.h"
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

static_assert(sizeof(struct stack_info) == Stack_ctx_words * sizeof(value), "");

static _Atomic int64_t fiber_id = 0;
static atomic_uintnat live_stack_counter = 0;

uintnat caml_live_stacks_memory (void) {
  return atomic_load(&live_stack_counter);
}

Caml_inline uintnat stack_bytes(struct stack_info* stack)
{
  return (uintnat)((char*)(stack->handler + 1) - (char*)stack);
}

extern _Atomic uintnat caml_custom_minor_ratio; /* see custom.c */

/* Count the memory of a fiber stack towards the minor GC's pace, as for a
   young custom block. */
static void account_new_fiber_stack(struct stack_info* stack)
{
  caml_adjust_minor_gc_speed(
    stack_bytes(stack),
    Bsize_wsize(Caml_state->minor_heap_wsz) / 100
      * atomic_load_relaxed(&caml_custom_minor_ratio));
}

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
    caml_gc_log ("Changing stack limit to %" CAML_PRIuNAT "k bytes",
                 new_max_wsize * sizeof (value) / 1024);
  }
  caml_max_stack_wsize = new_max_wsize;
}


uintnat caml_current_stack_size(void) {
  struct stack_info *current_stack = Caml_state->current_stack;
  return (Stack_high(current_stack) - (value*)current_stack->sp);
}

#define NUM_STACK_SIZE_CLASSES 5

struct stack_info** caml_alloc_stack_cache (void)
{
  struct stack_info** stack_cache =
    (struct stack_info**)caml_stat_alloc_noexc(sizeof(struct stack_info*) *
                                               NUM_STACK_SIZE_CLASSES);
  if (stack_cache == NULL)
    return NULL;

  for (int i = 0; i < NUM_STACK_SIZE_CLASSES; i++)
    stack_cache[i] = NULL;

  return stack_cache;
}

Caml_inline struct stack_info* alloc_for_stack (mlsize_t wosize)
{
  size_t stack_len = sizeof(struct stack_info) + sizeof(value) * wosize;
  size_t len;

  /* Some platforms require 16-byte alignment of the stack pointer, which
     will be _at the end_ of this allocation, so we need to ask for a bit more
     memory to make sure that
       caml_round_up(allocated stack base + stack_len, 16) + sizeof handler
     will fit the allocated space.

     When using mmap, we can rely upon the stack base being page-aligned
     and thus aligned to a 16 byte boundary, and can round up here;
     otherwise we need to always ask for 15 more bytes in order to cope with
     all misalignment possibilities, even though it is likely that the
     result of caml_stat_alloc_noexc() will be at least aligned to an
     8-byte boundary. */
#ifdef USE_MMAP_MAP_STACK
  len = caml_round_up(stack_len, 16) + sizeof(struct stack_handler);
#else
  len = stack_len + (16 - 1) + sizeof(struct stack_handler);
#endif

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
  CAMLassert(wosize>=size_bucket_wsz/2);
  return -1;
}

static struct stack_info*
alloc_size_class_stack_noexc(mlsize_t wosize, int cache_bucket, value hval,
                             value hexn, value heff, int64_t id)
{
  struct stack_info* stack;
  struct stack_handler* hand;
  struct stack_info **cache = Caml_state->stack_cache;

  static_assert(sizeof(struct stack_info) % sizeof(value) == 0, "");
  static_assert(sizeof(struct stack_handler) % sizeof(value) == 0, "");

  CAMLassert(cache != NULL);

  if (cache_bucket != -1 &&
      cache[cache_bucket] != NULL) {
    stack = cache[cache_bucket];
    cache[cache_bucket] =
      /* In the cache, reuse the exception_ptr field as an internal
         linked-list next pointer. */
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

    /* Ensure 16-byte alignment because some architectures (e.g. arm64)
       require it. alloc_for_stack() has allocated extra room to prevent
       this computation from overflowing. */
    hand = (struct stack_handler*)caml_round_up(
      (uintnat)stack + sizeof(struct stack_info) + sizeof(value) * wosize, 16);
    stack->handler = hand;
    atomic_fetch_add(&live_stack_counter,
                     (value*)(stack->handler+1) - (value*)stack);

  }

  hand->handle_value = hval;
  hand->handle_exn = hexn;
  hand->handle_effect = heff;
  hand->parent = NULL;
  stack->sp = (value*)hand;
  stack->exception_ptr = NULL;
  stack->id = id;
  stack->is_major_charged = false;
#ifdef DEBUG
  stack->magic = 42;
#endif
  /* Due to stack alignment performed above, the actual stack size may be
   * larger than requested. */
  CAMLassert(Stack_high(stack) - Stack_base(stack) >= wosize);
  return stack;

}

/* allocate a stack with at least "wosize" usable words of stack */
struct stack_info*
caml_alloc_stack_noexc(mlsize_t wosize, value hval, value hexn, value heff,
                       int64_t id)
{
  int cache_bucket = stack_cache_bucket (wosize);
  return alloc_size_class_stack_noexc(wosize, cache_bucket, hval, hexn, heff,
                                      id);
}

#ifdef NATIVE_CODE

value caml_alloc_stack (value hval, value hexn, value heff) {
  const int64_t id = atomic_fetch_add(&fiber_id, 1);
  bool cached = Caml_state->stack_cache[0] != NULL;
  struct stack_info* stack =
    alloc_size_class_stack_noexc(caml_fiber_wsz, 0 /* first bucket */,
                                 hval, hexn, heff, id);

  if (!stack) caml_raise_out_of_memory();
  if (!cached) account_new_fiber_stack(stack);

  fiber_debug_log ("Allocate stack=%p of %" CAML_PRIuNAT "words",
                   stack, caml_fiber_wsz);

  return Val_ptr(stack);
}


void caml_get_stack_sp_pc (struct stack_info* stack,
                           char** sp /* out */, uintnat* pc /* out */)
{
  char* p = (char*)stack->sp;
  p = First_frame(p);
  *pc = Saved_return_address(p); /* ret addr */
  *sp = p;                       /* pointer to first frame */
}

Caml_inline void scan_stack_frames(
  scanning_action f, scanning_action_flags fflags, void* fdata,
  struct stack_info* stack, value* gc_regs)
{
  char * sp;
  uintnat retaddr;
  value * regs;
  frame_descr * d;
  int n, ofs;
  unsigned short * p;
  value *root;
  caml_frame_descrs * fds = caml_get_frame_descrs();

  sp = (char*)stack->sp;
  regs = gc_regs;

next_chunk:
  if (sp == (char*)Stack_high(stack)) return;
  sp = First_frame(sp);
  retaddr = Saved_return_address_raw(sp);

  while(1) {
#ifdef Already_scanned
      if ((fflags & SCANNING_ONLY_RECENT_FRAMES) != 0) {
        /* Stop here if the frame has been scanned during earlier GCs  */
        if (Already_scanned(sp, retaddr)) break;
        /* Mark frame as already scanned */
        Mark_scanned(sp, retaddr);
      } else {
        /* Ignore mark and continue */
        retaddr = Mask_already_scanned(retaddr);
      }
#endif
    d = caml_find_frame_descr(fds, retaddr);
    CAMLassert(d);
    if (!frame_return_to_C(d)) {
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
      sp += frame_size(d);
      retaddr = Saved_return_address_raw(sp);
    } else {
      /* This marks the top of an ML stack chunk. Move sp to the previous
       * stack chunk.  */
      regs = Saved_gc_regs(sp); /* update gc_regs */
      sp += Stack_header_size;  /* skip trap frame, gc_regs, DWARF pointer */
      goto next_chunk;
    }
  }
}

void caml_scan_stack(
  scanning_action f, scanning_action_flags fflags, void* fdata,
  struct stack_info* stack, value* gc_regs)
{
  struct stack_info* starting_stack = stack;

  while (stack != NULL) {
    scan_stack_frames(f, fflags, fdata, stack, gc_regs);

    f(fdata, Stack_handle_value(stack), &Stack_handle_value(stack));
    f(fdata, Stack_handle_exception(stack), &Stack_handle_exception(stack));
    f(fdata, Stack_handle_effect(stack), &Stack_handle_effect(stack));

    stack = Stack_parent(stack);
    if (stack == starting_stack) break;  /* loop detected */
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
    /* Ensure there is at least one gc_regs bucket available before
       running any OCaml code. See fiber.h for documentation. */
    value* bucket = caml_stat_alloc(sizeof(value) * Wosize_gc_regs);
    bucket[0] = 0; /* no next bucket */
    Caml_state->gc_regs_buckets = bucket;
  }
}

#else /* End NATIVE_CODE, begin BYTE_CODE */

value caml_global_data = Val_unit;

CAMLprim value caml_alloc_stack(value hval, value hexn, value heff)
{
  value* sp;
  const int64_t id = atomic_fetch_add(&fiber_id, 1);
  bool cached = Caml_state->stack_cache[0] != NULL;
  struct stack_info* stack =
    alloc_size_class_stack_noexc(caml_fiber_wsz, 0 /* first bucket */,
                                 hval, hexn, heff, id);

  if (!stack) caml_raise_out_of_memory();
  if (!cached) account_new_fiber_stack(stack);

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
  value *low, *high;

  struct stack_info* starting_stack = stack;

  while (stack != NULL) {
    CAMLassert(stack->magic == 42);

    high = Stack_high(stack);
    low = stack->sp;
    for (value *sp = low; sp < high; sp++) {
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
    if (stack == starting_stack) break;  /* loop detected */
  }
}

#endif /* end BYTE_CODE */

/*
  Stack management.

  Used by the interpreter to allocate stack space.
*/

#ifdef NATIVE_CODE
/* Update absolute exception pointers for new stack*/
void caml_rewrite_exception_stack(struct stack_info *old_stack,
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

#ifdef WITH_FRAME_POINTERS

/* One link of the frame-pointer chain. */
struct stack_frame {
  struct stack_frame* prev;
  void* retaddr;
};

/* Relocate by [delta] the frame pointers of the OCaml frames lying above the
   C frame described by [link], for as long as they point inside [old_stack].
   The OCaml frames have already been copied to the new stack, [link->sp] has
   not been relocated yet. */
Caml_inline void relocate_frame_pointers(struct c_stack_link* link,
                                         struct stack_info* old_stack,
                                         ptrdiff_t delta)
{
#if defined(TARGET_power)
  /* On Power the c_stack_link sits above the ABI's 32-byte reserved area, so
     the OCaml frame record is not adjacent to it.  Reach it through
     [link->sp], which is the address of the innermost OCaml frame record,
     relocated by [delta] since the stack has already been copied. */
  struct stack_frame* fp = (struct stack_frame*)((char*)link->sp + delta);
#elif defined(TARGET_riscv)
  /* On RISC-V, the frame pointer s0 = CFA = sp + frame_size, pointing to the
     address just AFTER the {saved_s0, saved_ra} frame record.  So fp->prev
     gives a CFA value, and the actual frame record is at
     (CFA - sizeof(struct stack_frame)).  ENTER_FUNCTION sets s0 = sp + 16,
     so the saved s0 in the C function's frame equals link->sp + 16. */
  struct stack_frame* fp = ((struct stack_frame*)link) - 1;
  CAMLassert(fp->prev == (struct stack_frame*)((char*)link->sp + 16));
#else /* AMD64 and ARM64 */
  /* The frame pointer is pushed just below the c_stack_link.  This is
     somewhat tricky to guarantee when there are stack arguments to C calls:
     see caml_c_call_copy_stack_args */
  struct stack_frame* fp = ((struct stack_frame*)link) - 1;
  CAMLassert(fp->prev == (struct stack_frame*)link->sp);
#endif

#if defined(TARGET_riscv)
  while ((value*)fp->prev > Stack_base(old_stack) &&
         (value*)fp->prev <= Stack_high(old_stack)) {
    fp->prev = (struct stack_frame*)((char*)fp->prev + delta);
    fp = fp->prev - 1; /* CFA - 16 = frame record */
  }
#else
  while (Stack_base(old_stack) <= (value*)fp->prev &&
         (value*)fp->prev < Stack_high(old_stack)) {
    fp->prev = (struct stack_frame*)((char*)fp->prev + delta);
    fp = fp->prev;
  }
#endif
}

#endif /* WITH_FRAME_POINTERS */

int caml_try_realloc_stack(asize_t required_space)
{
  struct stack_info *old_stack, *new_stack;
  asize_t wsize;
  int stack_used;
  CAMLnoalloc;

  old_stack = Caml_state->current_stack;
  stack_used = Stack_high(old_stack) - (value*)old_stack->sp;
  wsize = Stack_high(old_stack) - Stack_base(old_stack);
  uintnat max_stack_wsize = caml_max_stack_wsize;
  wsize = wsize & (~1); // zero alignment bit
  do {
    if (wsize >= max_stack_wsize) return 0;
    wsize *= 2;
  } while (wsize < stack_used + required_space);

  if (wsize > 4096 / sizeof(value)) {
    caml_gc_log ("Growing stack to %" CAML_PRIuNAT "k bytes",
                 (uintnat) wsize * sizeof(value) / 1024);
  } else {
    caml_gc_log ("Growing stack to %" CAML_PRIuNAT " bytes",
                 (uintnat) wsize * sizeof(value));
  }

  new_stack = caml_alloc_stack_noexc(wsize,
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
  caml_rewrite_exception_stack(old_stack, (value**)&Caml_state->exn_handler,
                              new_stack);
#endif

  /* Update stack pointers in Caml_state->c_stack. It is possible to have
   * multiple c_stack_links to point to the same stack since callbacks are run
   * on existing stacks. */
  {
    ptrdiff_t delta =
      (char*)Stack_high(new_stack) - (char*)Stack_high(old_stack);

    for (struct c_stack_link *link = Caml_state->c_stack;
         link != NULL;
         link = link->prev) {
      if (link->stack == old_stack) {
#ifdef WITH_FRAME_POINTERS
        relocate_frame_pointers(link, old_stack, delta);
#endif
        link->stack = new_stack;
        link->sp = (char*)link->sp + delta;
      }
    }
  }

  /* Count fibers only, not the main stack of a domain. */
  if (Stack_parent(new_stack) != NULL) account_new_fiber_stack(new_stack);
  /* A stack already counted towards the major GC's pace stays counted;
     only its growth is added. */
  if (old_stack->is_major_charged) {
    new_stack->is_major_charged = true;
    caml_adjust_gc_speed(stack_bytes(new_stack) - stack_bytes(old_stack), 0);
  }
  caml_free_stack(old_stack);
  Caml_state->current_stack = new_stack;
  return 1;
}

struct stack_info* caml_alloc_main_stack (uintnat init_wsize)
{
  const int64_t id = atomic_fetch_add(&fiber_id, 1);
  struct stack_info* stk =
    caml_alloc_stack_noexc(init_wsize, Val_unit, Val_unit, Val_unit, id);
  return stk;
}

atomic_uintnat caml_cache_stacks_per_class =
#if defined(USE_MMAP_MAP_STACK)
  1
#else
  128
#endif
  ;

void caml_free_stack_memory(struct stack_info* stack) {
  atomic_fetch_sub(&live_stack_counter,
                   (value*)(stack->handler+1) - (value*)stack);
#ifdef DEBUG
  memset(stack, 0x42, (char*)stack->handler - (char*)stack);
#endif
#ifdef USE_MMAP_MAP_STACK
  munmap(stack, stack->size);
#else
  caml_stat_free(stack);
#endif
}

void caml_free_stack_cache (struct stack_info** stack_cache)
{
  if (stack_cache == NULL)
    return;

  struct stack_info* stored_stack;
  for (int i = 0; i < NUM_STACK_SIZE_CLASSES; i++) {
    while (stack_cache[i] != NULL) {
      stored_stack = stack_cache[i];
      stack_cache[i] = (struct stack_info*) stored_stack->exception_ptr;
      caml_free_stack_memory(stored_stack);
    };
  }
  caml_stat_free(stack_cache);
}

void caml_free_stack (struct stack_info* stack)
{
  CAMLnoalloc;
  struct stack_info** cache = Caml_state->stack_cache;

  CAMLassert(stack->magic == 42);
  CAMLassert(cache != NULL);
  if (stack->cache_bucket != -1) {
    struct stack_info* top = (struct stack_info*)cache[stack->cache_bucket];
    /* When stored inside the cache, the fiber id field is reused to count
       the number of fibers in the bucket */
    int64_t count = top ? top->id : 0;
    if (count < caml_cache_stacks_per_class) {
      /* Reuse exception_ptr to point to the next fiber in the bucket. */
      stack->exception_ptr = (void *)top;
      stack->id = count + 1;
      cache[stack->cache_bucket] = stack;
#ifdef DEBUG
    memset(Stack_base(stack), 0x42,
           (Stack_high(stack)-Stack_base(stack))*sizeof(value));
#endif
    } else {
      caml_free_stack_memory(stack);
    }
  } else {
    caml_free_stack_memory(stack);
  }
}

/* Reclaiming the stacks of unreachable continuations. See fiber.h. */

uintnat caml_continuation_charge_stacks(struct stack_info* cont_tail)
{
  uintnat bytes = 0;
  struct stack_info* stack = cont_tail;
  do {
    if (!stack->is_major_charged) {
      bytes += stack_bytes(stack);
      stack->is_major_charged = true;
    }
    stack = Stack_parent(stack);
  } while (stack != cont_tail && stack != NULL);
  return bytes;
}

/* The CAS to [Val_ptr(NULL)] claims [cont], so its stacks are freed once. */
void caml_free_dead_continuation(value cont)
{
  CAMLnoalloc;
  /* A borrow neither allocates nor polls, so a dead continuation is never
     borrowed. */
  value v = atomic_load_relaxed(Op_atomic_val(cont));
  CAMLassert(v != Val_cont_borrowed);
  if (v == Val_ptr(NULL)
      || !atomic_compare_exchange_strong(Op_atomic_val(cont), &v,
                                         Val_ptr(NULL)))
    return;
  if (caml_runtime_warnings_active())
    fprintf(stderr, "[ocaml] continuation dies without being resumed\n");
  struct stack_info* cont_tail = Ptr_val(v);
  struct stack_info* stack = Stack_parent(cont_tail);
  /* A captured chain is a cycle from [cont_tail] to its head: cut it there. */
  Stack_parent(cont_tail) = NULL;
  while (stack != NULL) {
    struct stack_info* next = Stack_parent(stack);
    fiber_debug_log("Free stack=%p of unreachable continuation", stack);
    caml_free_stack(stack);
    stack = next;
  }
}

/* The table of young continuations (fiber.h): [Young_conts_initial] entries,
   and a reserve that [perform] uses past the threshold. */
#define Young_conts_initial 256
#define Young_conts_reserve 64
/* After compaction, the table doubles if more than this fraction of it
   is still in use. */
#define Young_conts_grow_fraction 4

/* The word below [young_conts_base] is a sentinel entry that is never taken,
   so [caml_perform] reads the top entry without a bounds check. */
static const value young_conts_sentinel_field = 0;

static void set_young_conts(caml_domain_state* domain, value* base,
                            uintnat size)
{
  domain->young_conts_base = domain->young_conts_ptr = base;
  domain->young_conts_limit = base == NULL ? NULL : base + size;
  domain->young_conts_end =
    base == NULL ? NULL : base + size + Young_conts_reserve;
}

Caml_inline uintnat young_conts_size(caml_domain_state* domain)
{
  return domain->young_conts_end - domain->young_conts_base
         - Young_conts_reserve;
}

/* Allocate the table, or resize it to [size] entries keeping the entries
   it holds. Nothing points into the table, so it can move. */
static bool resize_young_conts(caml_domain_state* domain, uintnat size)
{
  value* base = domain->young_conts_base;
  uintnat used = base == NULL ? 0 : domain->young_conts_ptr - base;
  value* t = caml_stat_resize_noexc(base == NULL ? NULL : base - 1,
                 (1 + size + Young_conts_reserve) * sizeof(value));
  if (t == NULL) return false;
  t[0] = (value)&young_conts_sentinel_field;
  set_young_conts(domain, t + 1, size);
  domain->young_conts_ptr = t + 1 + used;
  return true;
}

int caml_init_young_conts(caml_domain_state* domain)
{
  CAMLassert(domain->young_conts_base == NULL);
  return resize_young_conts(domain, Young_conts_initial) ? 0 : -1;
}

void caml_free_young_conts(caml_domain_state* domain)
{
  CAMLassert(domain->young_conts_ptr == domain->young_conts_base);
  if (domain->young_conts_base != NULL)
    caml_stat_free(domain->young_conts_base - 1);
  set_young_conts(domain, NULL, 0);
}

void caml_compact_young_conts(caml_domain_state* domain)
{
  value* base = domain->young_conts_base;
  value* q = base;
  /* Keep the entries that are not taken, in order. Other domains may take
     one at any time, hence the atomic load; a take is final. */
  for (value* p = base; p < domain->young_conts_ptr; p++) {
    value cont = *p;
    if (atomic_load_relaxed(Op_atomic_val(cont)) != Val_ptr(NULL)) *q++ = cont;
  }
  uintnat size = young_conts_size(domain);
  domain->young_conts_ptr = q;
  domain->young_conts_limit = base + size;
  if ((uintnat)(q - base) > size / Young_conts_grow_fraction)
    (void)resize_young_conts(domain, 2 * size);
  /* Still full: stay in the reserve and request a minor GC, which empties
     the table; [caml_reset_young_limit] keeps the interrupt armed until
     then. */
  if (domain->young_conts_ptr >= domain->young_conts_limit) {
    domain->young_conts_limit = domain->young_conts_end;
    caml_request_minor_gc();
  }
}

void caml_free_dead_young_continuations(caml_domain_state* domain)
{
  value* base = domain->young_conts_base;
  value* ptr = domain->young_conts_ptr;
  if (base == NULL) return;
  /* After the end barrier, no domain points into our minor heap. A promoted
     continuation may be in use on another domain, so it is left alone. */
  for (value* p = base; p < ptr; p++) {
    value cont = *p;
    CAMLassert((value*)cont > domain->young_start
               && (value*)cont < domain->young_end);
    if (!Is_promoted_hd(Hd_val(cont))) caml_free_dead_continuation(cont);
  }
  /* Empty the table; it keeps its size. */
  set_young_conts(domain, base, young_conts_size(domain));
}

void caml_free_gc_regs_buckets(value *gc_regs_buckets)
{
  while (gc_regs_buckets != NULL) {
    value *next = (value*)gc_regs_buckets[0];
    caml_stat_free(gc_regs_buckets);
    gc_regs_buckets = next;
  }
}


CAMLprim value caml_continuation_use_noexc (value cont)
{
  value v;
  value null_stk = Val_ptr(NULL);
  CAMLnoalloc;

  fiber_debug_log("cont: is_block(%d) tag_val(%ul) is_young(%d)",
                  Is_block(cont), Tag_val(cont), Is_young(cont));
  CAMLassert(Is_block(cont));
  CAMLassert(Tag_val(cont) == Cont_tag);

  /* this forms a barrier between execution and any other domains
     that might be marking this continuation */
  if (!Is_young(cont) && caml_marking_started())
    caml_darken_cont(cont);

  if (caml_domain_alone()) {
    v = Field(cont, 0);
    CAMLassert(v != Val_cont_borrowed);
    Field(cont, 0) = null_stk;
    return v;
  }

  while (1) {
    v = atomic_load_acquire(Op_atomic_val(cont));
    if (v == null_stk) return null_stk;
    if (v == Val_cont_borrowed) {
      /* Borrowed: wait. The borrower neither allocates nor polls, so this
         cannot deadlock. */
      cpu_relax();
      continue;
    }
    if (atomic_compare_exchange_strong(Op_atomic_val(cont), &v, null_stk))
      return v;
  }
}

struct stack_info* caml_continuation_borrow(value cont)
{
  CAMLassert(Is_block(cont) && Tag_val(cont) == Cont_tag);
  /* Mark the continuation before borrowing it, so that no marking domain
     scans it while it is borrowed. */
  if (!Is_young(cont) && caml_marking_started())
    caml_darken_cont(cont);
  while (1) {
    value v = atomic_load_acquire(Op_atomic_val(cont));
    if (v == Val_ptr(NULL)) caml_raise_continuation_already_resumed();
    if (v == Val_cont_borrowed) { cpu_relax(); continue; }
    if (atomic_compare_exchange_strong(Op_atomic_val(cont), &v,
                                       Val_cont_borrowed))
      return Ptr_val(v);
  }
}

void caml_continuation_unborrow(value cont, struct stack_info* stk)
{
  CAMLassert(Field(cont, 0) == Val_cont_borrowed);
  atomic_store_release(Op_atomic_val(cont), Val_ptr(stk));
}

CAMLprim value caml_continuation_use (value cont)
{
  value v = caml_continuation_use_noexc(cont);
  if (v == Val_ptr(NULL))
    caml_raise_continuation_already_resumed();
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
  Stack_handle_value(stk) = hval;
  Stack_handle_exception(stk) = hexn;
  Stack_handle_effect(stk) = heff;
  return stack;
}

static const value * _Atomic caml_unhandled_effect_exn = NULL;
static const value * _Atomic caml_continuation_already_resumed_exn = NULL;

static const value * cache_named_exception(const value * _Atomic * cache,
                                           const char * name)
{
  const value * exn;
  exn = atomic_load_acquire(cache);
  if (exn == NULL) {
    exn = caml_named_value(name);
    if (exn == NULL) {
      fprintf(stderr, "Fatal error: exception %s\n", name);
      exit(2);
    }
    atomic_store_release(cache, exn);
  }
  return exn;
}

CAMLexport void caml_raise_continuation_already_resumed(void)
{
  const value * exn =
    cache_named_exception(&caml_continuation_already_resumed_exn,
                          "Effect.Continuation_already_resumed");
  caml_raise(*exn);
}

value caml_make_unhandled_effect_exn (value effect)
{
  CAMLparam1(effect);
  value res;
  const value * exn =
    cache_named_exception(&caml_unhandled_effect_exn, "Effect.Unhandled");
  res = caml_alloc_small(2,0);
  Field(res, 0) = *exn;
  Field(res, 1) = effect;
  CAMLreturn(res);
}

CAMLexport void caml_raise_unhandled_effect (value effect)
{
  caml_raise(caml_make_unhandled_effect_exn(effect));
}
