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

static __thread int stack_is_saved = 0;

#ifdef NATIVE_CODE

static struct stack_info* save_stack ()
{
  caml_domain_state* domain_state = Caml_state;
  struct stack_info* old_stack = domain_state->current_stack;
  return old_stack;
}

static void load_stack (struct stack_info* stack) {
  caml_domain_state* domain_state = Caml_state;
  domain_state->stack_threshold = Stack_base(stack)
    + caml_params->profile_slop_wsz + Stack_threshold / sizeof(value);
  domain_state->stack_high = Stack_high(stack);
  domain_state->current_stack = stack;
}

extern void caml_fiber_exn_handler (value) Noreturn;
extern void caml_fiber_val_handler (value) Noreturn;

#define INIT_FIBER_USED (3 + sizeof(struct caml_context) / sizeof(value))

value caml_alloc_stack (value hval, value hexn, value heff) {
  CAMLparam3(hval, hexn, heff);
  struct stack_info* stack;
  char* sp;
  struct caml_context *ctxt;

  stack = caml_stat_alloc(sizeof(value) * (caml_fiber_wsz + caml_params->profile_slop_wsz));
  stack->wosize = caml_fiber_wsz + caml_params->profile_slop_wsz;
  stack->magic = 42;
  Stack_handle_value(stack) = hval;
  Stack_handle_exception(stack) = hexn;
  Stack_handle_effect(stack) = heff;
  Stack_parent(stack) = NULL;

  sp = (char*)Stack_high(stack);
  /* Fiber exception handler that returns to parent */
  sp -= sizeof(value);
  *(value**)sp = (value*)caml_fiber_exn_handler;
  /* No previous exception frame. Infact, this is the debugger slot. Initialize it. */
  sp -= sizeof(value);
  Stack_debugger_slot(stack) = Stack_debugger_slot_offset_to_parent_slot(stack);
  /* Value handler that returns to parent */
  sp -= sizeof(value);
  *(value**)sp = (value*)caml_fiber_val_handler;

  /* Build a context */
  sp -= sizeof(struct caml_context);
  ctxt = (struct caml_context*)sp;
  ctxt->exception_ptr_offset = 2 * sizeof(value);
  ctxt->gc_regs = NULL;
  Stack_sp(stack) = -INIT_FIBER_USED;

  caml_gc_log ("Allocate stack=%p of %lu words", stack, caml_fiber_wsz);

  CAMLreturn (Val_ptr(stack));
}

void caml_get_stack_sp_pc (struct stack_info* stack, char** sp /* out */, uintnat* pc /* out */)
{
  char* p = (char*)(Stack_high(stack) + Stack_sp(stack));

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

  if (Stack_sp(stack) == 0) return;
  sp = (char*)(Stack_high(stack) + Stack_sp(stack));

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
  uintnat stack_available;

  stack_available = Caml_state->current_stack->wosize
                  /* Stack_sp() is a -ve value in words */
                  + Stack_sp (Caml_state->current_stack)
                  - Stack_ctx_words;
  if (stack_available < caml_params->profile_slop_wsz
                      + 2 * Stack_threshold / sizeof(value))
    caml_realloc_stack (0, 0, 0);
}

void caml_update_gc_regs_slot (value* gc_regs)
{
  struct caml_context *ctxt;
  ctxt = (struct caml_context*) (Stack_high(Caml_state->current_stack)
                                 + Stack_sp(Caml_state->current_stack));
  ctxt->gc_regs = gc_regs;
}

/* Returns 1 if the target stack is a fresh stack to which control is switching
 * to. */
int caml_switch_stack(struct stack_info* stk)
{
  save_stack();
  load_stack(stk);
  if (Stack_sp(stk) == -INIT_FIBER_USED)
    return 1;
  return 0;
}

#else /* End NATIVE_CODE, begin BYTE_CODE */

caml_root caml_global_data;

static struct stack_info* save_stack ()
{
  caml_domain_state* domain_state = Caml_state;
  struct stack_info* old_stack = domain_state->current_stack;
  Stack_sp(old_stack) = domain_state->extern_sp - domain_state->stack_high;
  Assert(domain_state->stack_threshold ==
         Stack_base(old_stack) + caml_params->profile_slop_wsz + Stack_threshold / sizeof(value));
  Assert(domain_state->stack_high == Stack_high(old_stack));
  Assert(domain_state->extern_sp == domain_state->stack_high + Stack_sp(old_stack));
  return old_stack;
}

static void load_stack(struct stack_info* new_stack)
{
  caml_domain_state* domain_state = Caml_state;
  domain_state->stack_threshold =
    Stack_base(new_stack) + caml_params->profile_slop_wsz + Stack_threshold / sizeof(value);
  domain_state->stack_high = Stack_high(new_stack);
  domain_state->extern_sp = domain_state->stack_high + Stack_sp(new_stack);
  domain_state->current_stack = new_stack;
}

CAMLprim value caml_alloc_stack(value hval, value hexn, value heff)
{
  CAMLparam3(hval, hexn, heff);
  struct stack_info* stack;
  value* sp;
  value* high;

  stack = caml_stat_alloc(sizeof(value) * caml_fiber_wsz);
  stack->wosize = caml_fiber_wsz;
  stack->magic = 42;
  high = sp = Stack_high(stack);

  // ?
  sp -= 1;
  sp[0] = Val_long(1); /* trapsp ?? */

  Stack_sp(stack) = sp - high;
  Stack_handle_value(stack) = hval;
  Stack_handle_exception(stack) = hexn;
  Stack_handle_effect(stack) = heff;
  Stack_parent(stack) = NULL;

  CAMLreturn (Val_ptr(stack));
}

/*
  Find the stack that performed an effect, skipping over several stacks that
  reperformed the effect if necessary.

  Reverses the parent pointers to point
  performer -> delegator instead of
  delegator -> performer.
*/
struct stack_info* caml_find_performer(struct stack_info* stack)
{
  caml_domain_state* domain_state = Caml_state;
  struct stack_info* parent = domain_state->current_stack;
  Assert (parent != NULL);
  do {
    struct stack_info* delegator = Stack_parent(stack);
    Stack_parent(stack) = parent;
    parent = stack;
    stack = delegator;
  } while (stack != NULL);
  return parent;
}

CAMLprim value caml_ensure_stack_capacity(value required_space)
{
  asize_t req = Long_val(required_space);
  if (Caml_state->extern_sp - req < Stack_base(Caml_state->current_stack))
    caml_realloc_stack(req, 0, 0);
  return Val_unit;
}

void caml_change_max_stack_size (uintnat new_max_size)
{
  asize_t size = Caml_state->stack_high - Caml_state->extern_sp
                 + caml_params->profile_slop_wsz
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
  low = high + Stack_sp(stack);
  for (sp = low; sp < high; sp++) {
    f(fdata, *sp, sp);
  }
}

struct stack_info* caml_switch_stack(struct stack_info* stk)
{
  struct stack_info* s = save_stack();
  load_stack(stk);
  return s;
}

#endif /* end BYTE_CODE */

void caml_save_stack_gc()
{
  Assert(stack_is_saved >= 0);
  save_stack();
  ++stack_is_saved;
}

int caml_stack_is_saved ()
{
  return stack_is_saved;
}

void caml_restore_stack_gc()
{
  caml_domain_state* domain_state = Caml_state;
  Assert(stack_is_saved > 0);
  --stack_is_saved;
  if (stack_is_saved == 0)
    load_stack(domain_state->current_stack);
}

/*
  Stack management.

  Used by the interpreter to allocate stack space.
*/

int caml_on_current_stack(value* p)
{
  return Stack_base(Caml_state->current_stack) <= p && p < Caml_state->stack_high;
}

void caml_realloc_stack(asize_t required_space, value* saved_vals, int nsaved)
{
  CAMLparamN(saved_vals, nsaved);
  struct stack_info *old_stack, *new_stack;
  asize_t size;
  int stack_used;

  old_stack = save_stack();

  stack_used = -Stack_sp(old_stack);
  size = Stack_high(old_stack) - Stack_base(old_stack);
  do {
    if (size >= caml_max_stack_size) caml_raise_stack_overflow();
    size *= 2;
  } while (size < stack_used + required_space);
  if (size > 4096 / sizeof(value)) {
    caml_gc_log ("Growing stack to %"
                 ARCH_INTNAT_PRINTF_FORMAT "uk bytes",
                 (uintnat) size * sizeof(value) / 1024);
  } else {
    caml_gc_log ("Growing stack to %"
                 ARCH_INTNAT_PRINTF_FORMAT "u bytes",
                 (uintnat) size * sizeof(value));
  }

  new_stack = caml_stat_alloc(sizeof(value) * (Stack_ctx_words + size));
  new_stack->wosize = Stack_ctx_words + size;
  new_stack->magic = 42;
  memcpy(Stack_high(new_stack) - stack_used,
         Stack_high(old_stack) - stack_used,
         stack_used * sizeof(value));

  Stack_sp(new_stack) = Stack_sp(old_stack);
  Stack_handle_value(new_stack) = Stack_handle_value(old_stack);
  Stack_handle_exception(new_stack) = Stack_handle_exception(old_stack);
  Stack_handle_effect(new_stack) = Stack_handle_effect(old_stack);
  Stack_parent(new_stack) = Stack_parent(old_stack);
#ifdef NATIVE_CODE
  Stack_debugger_slot(new_stack) =
    Stack_debugger_slot_offset_to_parent_slot(new_stack);
#endif

  load_stack(new_stack);

  /* Reset old stack */
  Stack_sp(old_stack) = 0;

  CAMLreturn0;
}

static void caml_init_stack (struct stack_info* stack)
{
  Stack_sp(stack) = 0;
  Stack_handle_value(stack) = Val_long(0);
  Stack_handle_exception(stack) = Val_long(0);
  Stack_handle_effect(stack) = Val_long(0);
  Stack_parent(stack) = NULL;
}

struct stack_info* caml_alloc_main_stack (uintnat init_size)
{
  struct stack_info* stk;

  /* Create a stack for the main program.
     The GC is not initialised yet, so we use caml_alloc_shr
     which cannot trigger it */
  stk = caml_stat_alloc(sizeof(value) * init_size);
  stk->wosize = init_size;
  stk->magic = 42;
  caml_init_stack (stk);

  return stk;
}

void caml_init_main_stack ()
{
  struct stack_info* stack;
  stack = caml_alloc_main_stack (caml_params->profile_slop_wsz +
                            Stack_size/sizeof(value));
  load_stack(stack);
}

CAMLprim value caml_clone_continuation (value cont)
{
  caml_failwith("broke this");//FIXME
#if 0
  CAMLparam1(cont);
  CAMLlocal4(new_cont, prev_target, source, target);
  intnat bvar_stat;
  int stack_used;

  bvar_stat = caml_bvar_status(cont);
  if (bvar_stat & BVAR_EMPTY)
    caml_invalid_argument ("Obj.clone: continuation already taken");

  prev_target = Val_unit;
  caml_read_field(cont, 0, &source);

  do {
    Assert (Is_block (source) && Tag_val(source) == Stack_tag);

    /* Ensure that the stack remains 16-byte aligned. Note: Stack_high
     * always returns 16-byte aligned down address. */
    stack_used = -Stack_sp(source);
    target = caml_alloc (Wosize_val(source), Stack_tag);
    memcpy((void*)target, (void*)source, sizeof(value) * Stack_ctx_words);
    memcpy(Stack_high(target) - stack_used, Stack_high(source) - stack_used,
           stack_used * sizeof(value));

    if (prev_target == Val_unit) {
      new_cont = caml_bvar_create (target);
    } else {
      Stack_parent(prev_target) = target;
    }

    prev_target = target;
    source = Stack_parent(source);
  } while (source != Val_unit);

  CAMLreturn(new_cont);
#endif
}

void caml_restore_stack()
{
  load_stack(Caml_state->current_stack);
}

struct stack_info* caml_reverse_fiber_stack (struct stack_info* stack)
{
  struct stack_info* next;
  struct stack_info* prev = NULL;

  while (stack != NULL) {
    next = Stack_parent(stack);
    Stack_parent(stack) = prev;
    prev = stack;
    stack = next;
  }

  return prev;
}

CAMLprim value caml_continuation_create (value stk)
{
  return caml_alloc_1(Cont_tag, stk);
}

CAMLprim value caml_continuation_use (value cont)
{
  struct stack_info* stk;
  CAMLassert(Is_block(cont) && Tag_val(cont) == Cont_tag);
  if (Is_young(cont)) {
    stk = Ptr_val(Op_val(cont)[0]);
    Op_val(cont)[0] = Val_ptr(NULL);
    if (stk == NULL) caml_failwith("Continuation already used");
    return Val_ptr(stk);
  } else {
    value v;
    caml_darken_cont(cont);
    v = Op_val(cont)[0];
    if (v != Val_ptr(NULL) &&
        __sync_bool_compare_and_swap(Op_val(cont), v, Val_ptr(NULL))) {
      return v;
    } else {
      caml_failwith("Continuation already used");
    }
  }
}
