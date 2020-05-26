#define CAML_INTERNALS

#include <string.h>
#include <unistd.h>
#include "caml/misc.h"
#include "caml/fiber.h"
#include "caml/gc_ctrl.h"
#include "caml/instruct.h"
#include "caml/fail.h"
#include "caml/alloc.h"
#include "caml/platform.h"
#include "caml/fix_code.h"
#include "caml/minor_gc.h"
#include "caml/major_gc.h"
#include "caml/shared_heap.h"
#include "caml/memory.h"
#include "caml/startup_aux.h"
#ifdef NATIVE_CODE
#include "caml/stack.h"
#include "frame_descriptors.h"
#endif

/* allocate a stack with at least "wosize" usable words of stack */
static struct stack_info* alloc_stack_noexc(mlsize_t wosize, value hval, value hexn, value heff)
{
  struct stack_info* stack;
  struct stack_handler* hand;

  CAML_STATIC_ASSERT(sizeof(struct stack_info) % sizeof(value) == 0);
  CAML_STATIC_ASSERT(sizeof(struct stack_handler) % sizeof(value) == 0);

  stack = caml_stat_alloc_noexc(sizeof(struct stack_info) +
                          sizeof(value) * wosize +
                          8 /* for alignment */ +
                          sizeof(struct stack_handler));
  if (stack == NULL) {
    return NULL;
  }

  /* Ensure 16-byte alignment because some architectures require it */
  hand = (struct stack_handler*)
    (((uintnat)stack + sizeof(struct stack_info) + sizeof(value) * wosize + 8)
     & ((uintnat)-1 << 4));
  hand->handle_value = hval;
  hand->handle_exn = hexn;
  hand->handle_effect = heff;
  hand->parent = NULL;
  stack->handler = hand;
  stack->sp = (value*)hand;
  stack->magic = 42;
  CAMLassert(Stack_high(stack) - Stack_base(stack) == wosize ||
             Stack_high(stack) - Stack_base(stack) == wosize + 1);
  return stack;
}

#ifdef NATIVE_CODE

value caml_alloc_stack (value hval, value hexn, value heff) {
  struct stack_info* stack = alloc_stack_noexc(caml_fiber_wsz, hval, hexn, heff);

  if (!stack) caml_raise_out_of_memory();

  caml_gc_log ("Allocate stack=%p of %lu words", stack, caml_fiber_wsz);

  return Val_ptr(stack);
}

void caml_get_stack_sp_pc (struct stack_info* stack, char** sp /* out */, uintnat* pc /* out */)
{
  char* p = (char*)stack->sp;

  p += sizeof(struct caml_context) + sizeof(value);
  *sp = p;
  *pc = Saved_return_address(*sp);
}

void caml_scan_stack(scanning_action f, void* fdata, struct stack_info* stack)
{
  char * sp;
  uintnat retaddr;
  value * regs;
  frame_descr * d;
  uintnat h;
  int n, ofs;
  unsigned short * p;
  value *root;
  struct caml_context* context;
  caml_frame_descrs fds = caml_get_frame_descrs();

  f(fdata, Stack_handle_value(stack), &Stack_handle_value(stack));
  f(fdata, Stack_handle_exception(stack), &Stack_handle_exception(stack));
  f(fdata, Stack_handle_effect(stack), &Stack_handle_effect(stack));
  if (Stack_parent(stack) != NULL)
    caml_scan_stack(f, fdata, Stack_parent(stack));

  if (stack->sp == Stack_high(stack)) return;
  sp = (char*)stack->sp;

next_chunk:
  if (sp == (char*)Stack_high(stack)) return;
  context = (struct caml_context*)sp;
  regs = context->gc_regs;
  sp += sizeof(struct caml_context);

  if (sp == (char*)Stack_high(stack)) return;
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
      /* This marks the top of an ML stack chunk. Move sp to the previous stack
       * chunk. This includes skipping over the trap frame (2 words). */
      sp += 2 * sizeof(value);
      goto next_chunk;
    }
  }
}

void caml_maybe_expand_stack ()
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

caml_root caml_global_data;

CAMLprim value caml_alloc_stack(value hval, value hexn, value heff)
{
  struct stack_info* stack = alloc_stack_noexc(caml_fiber_wsz, hval, hexn, heff);
  value* sp;

  if (!stack) caml_raise_out_of_memory();

  sp = Stack_high(stack);
  // ?
  sp -= 1;
  sp[0] = Val_long(1); /* trapsp ?? */

  stack->sp = sp;

  return Val_ptr(stack);
}

CAMLprim value caml_ensure_stack_capacity(value required_space)
{
  asize_t req = Long_val(required_space);
  if (Caml_state->current_stack->sp - req < Stack_base(Caml_state->current_stack))
    if (!caml_try_realloc_stack(req))
      caml_raise_stack_overflow();
  return Val_unit;
}

void caml_change_max_stack_size (uintnat new_max_size)
{
  asize_t size = Stack_high(Caml_state->current_stack) - Caml_state->current_stack->sp
                 + Stack_threshold / sizeof (value);

  if (new_max_size < size) new_max_size = size;
  if (new_max_size != caml_max_stack_size){
    caml_gc_log ("Changing stack limit to %luk bytes",
                     new_max_size * sizeof (value) / 1024);
  }
  caml_max_stack_size = new_max_size;
}

/*
  Root scanning.

  Used by the GC to find roots on the stacks of running or runnable fibers.
*/

void caml_scan_stack(scanning_action f, void* fdata, struct stack_info* stack)
{
  value *low, *high, *sp;
  Assert(stack->magic == 42);

  f(fdata, Stack_handle_value(stack), &Stack_handle_value(stack));
  f(fdata, Stack_handle_exception(stack), &Stack_handle_exception(stack));
  f(fdata, Stack_handle_effect(stack), &Stack_handle_effect(stack));
  if (Stack_parent(stack))
    caml_scan_stack(f, fdata, Stack_parent(stack));

  high = Stack_high(stack);
  low = stack->sp;
  for (sp = low; sp < high; sp++) {
    f(fdata, *sp, sp);
  }
}

#endif /* end BYTE_CODE */

/*
  Stack management.

  Used by the interpreter to allocate stack space.
*/

#ifdef NATIVE_CODE
/* Update absolute exception pointers for new stack*/
static void rewrite_exception_stack(struct stack_info *old_stack, value** exn_ptr, struct stack_info *new_stack)
{
  caml_gc_log ("Old [%p, %p]", Stack_base(old_stack), Stack_high(old_stack));
  caml_gc_log ("New [%p, %p]", Stack_base(new_stack), Stack_high(new_stack));
  if(exn_ptr) {
    caml_gc_log ("*exn_ptr=%p", *exn_ptr);

    while (Stack_base(old_stack) < *exn_ptr && *exn_ptr <= Stack_high(old_stack)) {
      value* old_val = *exn_ptr;
      *exn_ptr = Stack_high(new_stack) - (Stack_high(old_stack) - *exn_ptr);

      caml_gc_log ("Rewriting %p to %p", old_val, *exn_ptr);

      CAMLassert(Stack_base(new_stack) < *exn_ptr);
      CAMLassert((value*)*exn_ptr <= Stack_high(new_stack));

      exn_ptr = (value**)*exn_ptr;
    }
    caml_gc_log ("finished with *exn_ptr=%p", *exn_ptr);
  } else {
    caml_gc_log ("exn_ptr is null");
  }
}
#endif

int caml_try_realloc_stack(asize_t required_space)
{
  struct stack_info *old_stack, *new_stack;
  asize_t size;
  int stack_used;
  CAMLnoalloc;

  old_stack = Caml_state->current_stack;
  stack_used = Stack_high(old_stack) - (value*)old_stack->sp;
#ifdef DEBUG
  size = stack_used + stack_used / 16 + required_space;
  if (size >= caml_max_stack_size) return 0;
#else
  size = Stack_high(old_stack) - Stack_base(old_stack);
  do {
    if (size >= caml_max_stack_size) return 0;
    size *= 2;
  } while (size < stack_used + required_space);
#endif
  if (size > 4096 / sizeof(value)) {
    caml_gc_log ("Growing stack to %"
                 ARCH_INTNAT_PRINTF_FORMAT "uk bytes",
                 (uintnat) size * sizeof(value) / 1024);
  } else {
    caml_gc_log ("Growing stack to %"
                 ARCH_INTNAT_PRINTF_FORMAT "u bytes",
                 (uintnat) size * sizeof(value));
  }

  new_stack = alloc_stack_noexc(size,
                                Stack_handle_value(old_stack),
                                Stack_handle_exception(old_stack),
                                Stack_handle_effect(old_stack));
  if (!new_stack) return 0;
  memcpy(Stack_high(new_stack) - stack_used,
         Stack_high(old_stack) - stack_used,
         stack_used * sizeof(value));
  new_stack->sp = Stack_high(new_stack) - stack_used;
  Stack_parent(new_stack) = Stack_parent(old_stack);
#ifdef NATIVE_CODE
  rewrite_exception_stack(old_stack, (value**)&Caml_state->exn_handler, new_stack);
#endif

  /* Update stack pointers in Caml_state->c_stack */
  {
    struct c_stack_link* link;
    for (link = Caml_state->c_stack; link; link = link->prev) {
#ifdef DEBUG
      struct stack_info* cb = NULL;
      /* Verify that all callback frames on this C stack belong to the
       same fiber */
      if (link->stack == NULL) {
        /* only possible for the first link, if it has not done any C calls yet */
        /* FIXME: at the moment, this is possible due to a couple of odd ways of entering C code
           (via GC, etc.) */
        //CAMLassert(link == Caml_state->c_stack);
      } else if (cb == NULL) {
        /* first non-NULL stack */
        cb = link->stack;
      } else {
        CAMLassert(link->stack == cb);
      }
#endif
      if (link->stack == old_stack) {
        link->stack = new_stack;
        link->sp = (void*)((char*)Stack_high(new_stack) - ((char*)Stack_high(old_stack) - (char*)link->sp));
      }
    }
  }

  caml_free_stack(old_stack);
  Caml_state->current_stack = new_stack;
  return 1;
}

struct stack_info* caml_alloc_main_stack (uintnat init_size)
{
  struct stack_info* stk =
    alloc_stack_noexc(init_size, Val_unit, Val_unit,  Val_unit);
  return stk;
}

void caml_free_stack (struct stack_info* stack)
{
  CAMLnoalloc;
  CAMLassert(stack->magic == 42);
#ifdef DEBUG
  memset(stack, 0x42, (char*)stack->handler - (char*)stack);
#endif
  caml_stat_free(stack);
}

CAMLprim value caml_clone_continuation (value cont)
{
  CAMLparam1(cont);
  CAMLlocal1(new_cont);
  intnat stack_used;
  struct stack_info *source, *orig_source, *target, *ret_stack;
  struct stack_info **link = &ret_stack;

  new_cont = caml_alloc_1(Cont_tag, Val_ptr(NULL));
  orig_source = source = Ptr_val(caml_continuation_use(cont));
  do {
    CAMLnoalloc;
    stack_used = Stack_high(source) - (value*)source->sp;
    target = alloc_stack_noexc(Stack_high(source) - Stack_base(source),
                               Stack_handle_value(source),
                               Stack_handle_exception(source),
                               Stack_handle_effect(source));
    if (!target) caml_raise_out_of_memory();
    memcpy(Stack_high(target) - stack_used, Stack_high(source) - stack_used,
           stack_used * sizeof(value));
#ifdef NATIVE_CODE
    {
      /* pull out the exception pointer from the caml context on the stack */
      value* exn_start =
        Stack_high(target) - (Stack_high(source) - (value*)source->sp);
      rewrite_exception_stack(source, (value**)exn_start, target);
    }
#endif
    target->sp = Stack_high(target) - stack_used;
    *link = target;
    link = &Stack_parent(target);
    source = Stack_parent(source);
  } while (source != NULL);
  caml_continuation_replace(cont, orig_source);
  caml_continuation_replace(new_cont, ret_stack);
  CAMLreturn (new_cont);
}

CAMLprim value caml_continuation_use (value cont)
{
  caml_gc_log("cont: is_block(%d) tag_val(%ul) is_minor(%d)", Is_block(cont), Tag_val(cont), Is_minor(cont));
  CAMLassert(Is_block(cont) && Tag_val(cont) == Cont_tag);

  value v;
  if (!Is_minor(cont) ) caml_darken_cont(cont);

  v = Op_val(cont)[0];
  if (v != Val_ptr(NULL) &&
      atomic_compare_exchange_strong(Op_atomic_val(cont), &v, Val_ptr(NULL))) {
    return v;
  } else {
    caml_invalid_argument("continuation already taken");
  }
}

void caml_continuation_replace(value cont, struct stack_info* stk)
{
  value n = Val_ptr(NULL);
  int b = atomic_compare_exchange_strong(Op_atomic_val(cont), &n, Val_ptr(stk));
  CAMLassert(b);
  (void)b; /* squash unused warning */
}
