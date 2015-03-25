#include <string.h>
#include <unistd.h>
#include "fiber.h"
#include "gc_ctrl.h"
#include "instruct.h"
#include "fail.h"
#include "alloc.h"
#include "platform.h"
#include "fix_code.h"

/* One word at the base of the stack is used to store the stack pointer */
#define Stack_ctx_words 2
#define Stack_base(stk) (Op_val(stk) + Stack_ctx_words)
#define Stack_high(stk) (Op_val(stk) + Wosize_val(stk))
#define Stack_sp(stk) (*(long*)(Op_val(stk) + 0))
#define Stack_dirty_domain(stk) (*(struct domain**)(Op_val(stk) + 1))

CAMLexport __thread value caml_current_stack;
CAMLexport __thread value * caml_stack_high; /* one-past-the-end */
CAMLexport __thread value * caml_stack_threshold; /* low + Stack_threshold */
CAMLexport __thread value * caml_extern_sp;

CAMLexport __thread intnat caml_trap_sp_off = 0;
CAMLexport __thread intnat caml_trap_barrier_off;
CAMLexport __thread intnat caml_extra_args;
CAMLexport __thread int caml_c_call_args;
CAMLexport __thread code_t caml_saved_pc;

caml_root caml_global_data;

static void dirty_stack(value);

static value save_stack ()
{
  value old_stack = caml_current_stack;
  Stack_sp(old_stack) = caml_extern_sp - caml_stack_high;
  Assert(caml_stack_threshold == Stack_base(old_stack) + Stack_threshold / sizeof(value));
  Assert(caml_stack_high == Stack_high(old_stack));
  Assert(caml_extern_sp == caml_stack_high + Stack_sp(old_stack));
  dirty_stack(old_stack);
  return old_stack;
}

static void load_stack(value newstack)
{
  Assert(Tag_val(newstack) == Stack_tag);
  caml_stack_threshold = Stack_base(newstack) + Stack_threshold / sizeof(value);
  caml_stack_high = Stack_high(newstack);
  caml_extern_sp = caml_stack_high + Stack_sp(newstack);
  caml_current_stack = newstack;
}

value caml_swap_stack(value newstack)
{
  value old_stack = save_stack();
  load_stack(newstack);

  /* dirty old_stack? */
  
  return old_stack;
}


static opcode_t thunk_code[] = { PUSHACC1, APPTERM1, 2 };
void caml_init_fibers ()
{
#ifdef THREADED_CODE
  caml_thread_code(thunk_code, sizeof(thunk_code));
#endif
}


#define Fiber_stack_wosize ((Stack_threshold / sizeof(value)) *2)

value caml_create_stack(value function)
{
  CAMLparam1(function);
  value stack = caml_alloc(Fiber_stack_wosize, Stack_tag);
  value* high = Stack_high(stack);
  value* sp = high;

  sp -= 5;
  sp[0] = Val_long(0); /* caml_trap_sp_off */
  sp[1] = Val_pc(thunk_code);  /* pc */
  sp[2] = Val_long(0); /* env */
  sp[3] = Val_long(0); /* extra_args */
  sp[4] = function;

  Stack_sp(stack) = sp - high;
  Stack_dirty_domain(stack) = 0;

  CAMLreturn(stack);
}


struct caml_runqueue* caml_init_runqueue()
{
  value stack;

  /* Create a stack for the main program.
     The GC is not initialised yet, so we use caml_alloc_shr
     which cannot trigger it */
  stack = caml_alloc_shr(Stack_size/sizeof(value), Stack_tag);
  Stack_sp(stack) = 0;
  Stack_dirty_domain(stack) = 0;
  load_stack(stack);

  return 0;
}



CAMLprim value caml_spawn(value clos)
{
  CAMLparam1(clos);
  CAMLlocal2(stack, cont);

  stack = caml_create_stack(clos);
  cont = caml_alloc_small(2, 0);
  Init_field(cont, 0, stack);
  Init_field(cont, 1, Val_int(0));
  CAMLreturn (cont);
}

/*
  Stack management.

  Used by the interpreter to allocate stack space.
*/

int caml_on_current_stack(value* p)
{
  return Stack_base(caml_current_stack) <= p && p < caml_stack_high;
}

void caml_realloc_stack(asize_t required_space, value* saved_vals, int nsaved)
{
  CAMLparamN(saved_vals, nsaved);
  CAMLlocal2(old_stack, new_stack);
  asize_t size;
  int stack_used;

  old_stack = save_stack();

  stack_used = -Stack_sp(old_stack);
  size = Stack_high(old_stack) - Stack_base(old_stack);
  do {
    if (size >= caml_max_stack_size) caml_raise_stack_overflow();
    size *= 2;
  } while (size < stack_used + required_space);
  caml_gc_log ("Growing stack to %"
                         ARCH_INTNAT_PRINTF_FORMAT "uk bytes",
                   (uintnat) size * sizeof(value) / 1024);

  new_stack = caml_alloc(Stack_ctx_words + size, Stack_tag);
  memcpy(Stack_high(new_stack) - stack_used,
         Stack_high(old_stack) - stack_used,
         stack_used * sizeof(value));

  Stack_sp(new_stack) = Stack_sp(old_stack);
  
  Stack_dirty_domain(new_stack) = 0;
  if (Stack_dirty_domain(old_stack)) {
    Assert (Stack_dirty_domain(old_stack) == caml_domain_self());
    Stack_dirty_domain(old_stack) = 0;
    dirty_stack(new_stack);
  }

  load_stack(new_stack);
  CAMLreturn0;
}

CAMLprim value caml_ensure_stack_capacity(value required_space)
{
  asize_t req = Long_val(required_space);
  if (caml_extern_sp - req < Stack_base(caml_current_stack))
    caml_realloc_stack(req, 0, 0);
  return Val_unit;
}

void caml_change_max_stack_size (uintnat new_max_size)
{
  asize_t size = caml_stack_high - caml_extern_sp
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

static __thread int stack_is_saved = 0;
void caml_save_stack_gc()
{
  Assert(!stack_is_saved);
  save_stack();
  stack_is_saved = 1;
}

void caml_restore_stack_gc()
{
  Assert(stack_is_saved);
  load_stack(caml_current_stack);
  stack_is_saved = 0;
}

static void dirty_stack(value stack)
{
  /* There is no write barrier (caml_modify) on writes to the stack,
     so a just-run fiber's stack may contain untracked shared->young
     pointers or pointers to unmarked objects. We add the stack to a
     ref table so that the GC can find these pointers. */
  if (!Is_minor(stack)) {
    Assert(Stack_dirty_domain(stack) == 0 ||
           Stack_dirty_domain(stack) == caml_domain_self());
    if (Stack_dirty_domain(stack) == 0) {
      Stack_dirty_domain(stack) = caml_domain_self();
      Ref_table_add(&caml_remembered_set.fiber_ref, stack, 0);
    }
  }
}

void caml_scan_dirty_stack(scanning_action f, value stack)
{
  Assert(Tag_val(stack) == Stack_tag);
  if (Stack_dirty_domain(stack) == caml_domain_self()) {
    caml_scan_stack(f, stack);
  }
}

void caml_clean_stack(value stack)
{
  Assert(Tag_val(stack) == Stack_tag);
  if (Stack_dirty_domain(stack) == caml_domain_self()) {
    Stack_dirty_domain(stack) = 0;
  }
}

void caml_scan_stack(scanning_action f, value stack)
{
  value *low, *high, *sp;
  Assert(Is_block(stack) && Tag_val(stack) == Stack_tag);

  if (Is_promoted_hd(Hd_val(stack)))
    Assert(!Is_young(stack)); //FIXME

  high = Stack_high(stack);
  low = high + Stack_sp(stack);
  for (sp = low; sp < high; sp++) {
    f(*sp, sp);
  }
}
