#include <string.h>
#include "fiber.h"
#include "gc_ctrl.h"
#include "instruct.h"
#include "fail.h"
#include "alloc.h"
#include "platform.h"
#include "fix_code.h"


/* 
   Fiber objects.

   Fibers are represented as normal blocks, with field indices
   given by the following enum.
*/

enum {
  FIBER_STACK,

  /* Whether the intepreter should quit when this fiber ends */
  FIBER_IS_MAIN,

  /* Value passed across context switch */
  FIBER_BLOCKVAL,
  
  /* Linked list (used in mvar wait queues) */
  FIBER_NEXT,

  FIBER_NFIELDS
};

/* 
   Fiber contexts .

   The context of a paused fiber is stored at the base of its
   stack in the following structure.
*/

struct fiber_ctx {
  intnat sp;
  intnat trap_sp;
  code_t pc;
  intnat extra_args;
  struct domain* domain;
  int dirty;
  int c_args;
};

#define Round_up_words(n) (((n) + sizeof(value) - 1) / sizeof(value) * sizeof(value))

#define Stack_ctx_size Round_up_words(sizeof(struct fiber_ctx))

#define Stack_ctx(fib) ((struct fiber_ctx*)fib)

#define Stack_base(fib) ((value*)((char*)fib + Stack_ctx_size))

/*
  Deque data structure.

  Used to represent the runqueues below.
*/

typedef struct {
  value* elems;
  int pos, len;
  int alloc_size; /* always a power of two */
} deque;
#define Deque_default_size 16

static void deque_init(deque* d)
{
  d->alloc_size = Deque_default_size;
  d->elems = caml_stat_alloc(sizeof(value) * d->alloc_size);
  d->pos = d->len = 0;
}

static void deque_grow(deque* d)
{
  if (d->len == d->alloc_size) {
    value* new_elems;
    int i;

    new_elems = caml_stat_alloc(sizeof(value) * d->alloc_size * 2);
    for (i = 0; i < d->len; i++) {
      new_elems[i] = d->elems[(d->pos + i) & (d->alloc_size - 1)];
    }
    caml_stat_free(d->elems);
    d->elems = new_elems;
    d->alloc_size *= 2;
    d->pos = 0;
  } 
}

/* add to tail of deque */
static void append(deque* d, value v)
{
  deque_grow(d);
  d->elems[(d->pos + d->len) & (d->alloc_size - 1)] = v;
  d->len++;
}

/* remove from tail of deque */
static value unappend(deque* d)
{
  Assert(d->len > 0);
  d->len--;
  return d->elems[(d->pos + d->len) & (d->alloc_size - 1)];
}

/* add to head of deque */
static void push(deque* d, value v)
{
  deque_grow(d);
  d->len++;
  d->pos = (d->pos - 1) & (d->alloc_size - 1);
  d->elems[d->pos] = v;
}

/* remove from head of deque */
static value pop(deque* d)
{
  Assert(d->len > 0);
  value v = d->elems[d->pos];
  d->pos = (d->pos + 1) & (d->alloc_size - 1);
  d->len--;
  return v;
}

static void deque_iter(scanning_action f, deque* d)
{
  int i;
  for (i = 0; i < d->len; i++) {
    value* fib = &d->elems[(d->pos + i) & (d->alloc_size - 1)];
    f(*fib, fib);
  }
}

/*
  Runqueue management.

  Implemented as a pair of ring buffers of fibers.
*/

#define Runqueue_default_size 16
struct caml_runqueue {
  value current; /* currently executing fiber */

  /* fibers woken from a different domain */
  caml_plat_mutex woken_lock;
  caml_root woken;


  deque front, back;
};

__thread struct caml_runqueue* caml_runqueue;

static void rq_init() {
  caml_runqueue = caml_stat_alloc(sizeof(struct caml_runqueue));
  caml_plat_mutex_init(&caml_runqueue->woken_lock);
  caml_runqueue->woken = caml_create_root(Val_unit);
  deque_init(&caml_runqueue->front);
  deque_init(&caml_runqueue->back);
}

static value find_more_work()
{
  CAMLparam0();
  CAMLlocal1(woken);
  int found = 0;
  struct caml_runqueue* rq = caml_runqueue;

  while (1) {
    With_mutex(&rq->woken_lock) {
      woken = caml_read_root(rq->woken);
      caml_modify_root(rq->woken, Val_unit);
    }
    
    while (woken != Val_unit) {
      value next = Field(woken, FIBER_NEXT);
      caml_modify_field(woken, FIBER_NEXT, Val_unit);
      append(&rq->back, woken);
      woken = next;
      found++;
    }
    if (found) caml_gc_log("Found %d incoming fibers", found);
    
    if (rq->back.len) {
      deque tmp = rq->front;
      rq->front = rq->back;
      rq->back = tmp;
      CAMLreturn (pop(&rq->front));
    }
    
    caml_domain_spin();
  }
}

static value next_fiber()
{
  if (caml_runqueue->front.len) 
    return pop(&caml_runqueue->front);
  else
    return find_more_work();
}


/*
  Context switching.

  The current interpreter context is stored in these thread-local
  variables, their values can be saved into and restored out of a
  fiber object.
*/

CAMLexport __thread value * caml_stack_high; /* one-past-the-end */
CAMLexport __thread value * caml_stack_threshold; /* low + Stack_threshold */
CAMLexport __thread value * caml_extern_sp;
CAMLexport __thread intnat caml_trap_sp_off;
CAMLexport __thread intnat caml_trap_barrier_off;
CAMLexport __thread intnat caml_extra_args;
CAMLexport __thread int caml_c_call_args;
CAMLexport __thread code_t caml_saved_pc;

caml_root caml_global_data;

static value load_context(value fib)
{
  value stack = Field(fib, FIBER_STACK);
  struct fiber_ctx* ctx = Stack_ctx(stack);
  Assert(Wosize_val(stack) >= Stack_threshold / sizeof(value));
  caml_stack_threshold = Stack_base(stack) + Stack_threshold / sizeof(value);
  caml_stack_high = (value*)stack + Wosize_val(stack);
  caml_extern_sp = caml_stack_high + ctx->sp;
  caml_trap_sp_off = ctx->trap_sp;
  caml_trap_barrier_off = Val_long(1);

  caml_c_call_args = ctx->c_args;
  caml_extra_args = ctx->extra_args;
  caml_saved_pc = ctx->pc;

  caml_runqueue->current = fib;

  return Field(fib, FIBER_BLOCKVAL);
}

static void dirty_stack(value stack);
static value save_context(value blockval)
{
  value fib = caml_runqueue->current;
  value stack = Field(fib, FIBER_STACK);
  struct fiber_ctx* ctx = Stack_ctx(stack);
  Assert(caml_stack_threshold == Stack_base(stack) + Stack_threshold / sizeof(value));
  Assert(caml_stack_high == (value*)stack + Wosize_val(stack));
  Assert(Field(fib, FIBER_NEXT) == Val_unit);

  ctx->sp = caml_extern_sp - caml_stack_high;
  ctx->trap_sp = caml_trap_sp_off;
  ctx->pc = caml_saved_pc;
  ctx->c_args = caml_c_call_args;
  ctx->extra_args = caml_extra_args;

  dirty_stack(stack);

  caml_modify_field(fib, FIBER_BLOCKVAL, blockval);

  return fib;
}

CAMLprim value caml_yield (value v)
{
  append(&caml_runqueue->back, save_context(v));
  return load_context(next_fiber());
}

/*
  Creating and terminating fibers.

  The main fiber is special, in that its termination causes
  the interpreter to quit. See the STOP instruction in interp.c.
*/

int caml_running_main_fiber()
{
  return Int_val(Field(caml_runqueue->current, FIBER_IS_MAIN));
}

value caml_fiber_death()
{
  caml_gc_log("Fiber died");
  return load_context(next_fiber());
}

struct caml_runqueue* caml_init_runqueue()
{
  value fib, stack;
  struct fiber_ctx* ctx;

  /* Initialise runqueue */
  rq_init();

  /* Create a fiber for the main program */
  stack = caml_alloc((Stack_ctx_size + Stack_size)/sizeof(value), Stack_tag);
  ctx = Stack_ctx(stack);
  ctx->sp = 0;
  ctx->domain = caml_domain_self();
  ctx->dirty = 0;
  ctx->c_args = 0;

  /* these fields are ignored */
  ctx->trap_sp = 0;
  ctx->pc = 0;
  ctx->extra_args = 0;

  fib = caml_alloc(FIBER_NFIELDS, 0);
  caml_initialize_field(fib, FIBER_STACK, stack);
  caml_initialize_field(fib, FIBER_IS_MAIN, Val_long(1));
  caml_initialize_field(fib, FIBER_BLOCKVAL, Val_unit); /* ignored */

  caml_initialize_field(fib, FIBER_NEXT, Val_unit);

  load_context(fib);
  return caml_runqueue;
}

#define Fiber_stack_wosize ((Stack_threshold / sizeof(value)) *2)


#ifdef THREADED_CODE
static opcode_t fiber_thunk_code[] = { APPLY1, STOP };
void caml_init_fibers ()
{
  caml_thread_code(fiber_thunk_code, sizeof(fiber_thunk_code));
}
#else
static const opcode_t fiber_thunk_code[] = { APPLY1, STOP };
void caml_init_fibers () {}
#endif


CAMLprim value caml_spawn(value clos /*, value arg */)
{
  CAMLparam1(clos);
  CAMLlocal2(fib, stack);
  value* sp;
  struct fiber_ctx* ctx;
  stack = caml_alloc(Stack_ctx_size / sizeof(value) + Fiber_stack_wosize, Stack_tag);
  sp = (value*)stack + Wosize_val(stack);
  sp[-1] = /* arg */ Val_unit;
  sp[-2] = Val_unit; /* fake env */
  ctx = Stack_ctx(stack);
  ctx->sp = -2; /* two items on stack */
  ctx->trap_sp = 0;
  ctx->pc = (code_t)fiber_thunk_code;
  ctx->extra_args = 0;
  ctx->domain = caml_domain_self();
  ctx->dirty = 0;
  ctx->c_args = 0;
  fib = caml_alloc(FIBER_NFIELDS, 0);
  caml_initialize_field(fib, FIBER_IS_MAIN, Val_long(0));
  caml_initialize_field(fib, FIBER_STACK, stack);
  caml_initialize_field(fib, FIBER_BLOCKVAL, clos);
  caml_initialize_field(fib, FIBER_NEXT, Val_unit);

  /* Switch to the newly spawned fiber immediately */
  push(&caml_runqueue->front, save_context(Val_unit));
  CAMLreturn (load_context(fib));
}

/*
  Stack management.

  Used by the interpreter to allocate stack space.
*/

int caml_on_current_stack(value* p)
{
  value stack = Field(caml_runqueue->current, FIBER_STACK);
  return Stack_base(stack) <= p && p < caml_stack_high;
}

void caml_realloc_stack(asize_t required_space, value* saved_vals, int nsaved)
{
  CAMLparamN(saved_vals, nsaved);
  CAMLlocal3(old_stack, new_stack, fib);
  asize_t size;
  int stack_used;

  fib = save_context(Val_unit);
  old_stack = Field(fib, FIBER_STACK);

  stack_used = -Stack_ctx(old_stack)->sp;
  size = (((value*)old_stack + Wosize_val(old_stack)) - Stack_base(old_stack));
  do {
    if (size >= caml_max_stack_size) caml_raise_stack_overflow();
    size *= 2;
  } while (size < stack_used + required_space);
  caml_gc_log ("Growing stack to %"
                         ARCH_INTNAT_PRINTF_FORMAT "uk bytes",
                   (uintnat) size * sizeof(value) / 1024);

  new_stack = caml_alloc(Stack_ctx_size / sizeof(value) + size, Stack_tag);
  memcpy((value*)new_stack + Wosize_val(new_stack) - stack_used,
         (value*)old_stack + Wosize_val(old_stack) - stack_used,
         stack_used * sizeof(value));

  *Stack_ctx(new_stack) = *Stack_ctx(old_stack); /* copy ctx struct */
  if (Stack_ctx(old_stack)->dirty) {
    Stack_ctx(old_stack)->dirty = 0;
    Stack_ctx(new_stack)->dirty = 0;
    dirty_stack(new_stack);
  }
  caml_modify_field(fib, FIBER_STACK, new_stack);

  load_context(fib);
  CAMLreturn0;
}

CAMLprim value caml_ensure_stack_capacity(value required_space)
{
  asize_t req = Long_val(required_space);
  if (caml_extern_sp - req < (value*)Field(caml_runqueue->current, FIBER_STACK))
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
  value fib;
  Assert(!stack_is_saved);
  fib = save_context(Val_unit);
  Assert(fib == caml_runqueue->current);
  stack_is_saved = 1;
}

void caml_restore_stack_gc()
{
  Assert(stack_is_saved);
  load_context(caml_runqueue->current);
  stack_is_saved = 0;
}

static void dirty_stack(value stack)
{
  /* There is no write barrier (caml_modify) on writes to the stack,
     so a just-run fiber's stack may contain untracked shared->young
     pointers or pointers to unmarked objects. We add the stack to a
     ref table so that the GC can find these pointers. */
  if (!Is_minor(stack)) {
    if (!Stack_ctx(stack)->dirty) {
      Stack_ctx(stack)->dirty = 1;
      Ref_table_add(&caml_remembered_set.fiber_ref, stack, 0);
    }
  }
}

void caml_scan_dirty_stack(scanning_action f, value stack)
{
  struct fiber_ctx* ctx = Stack_ctx(stack);
  Assert(Tag_val(stack) == Stack_tag);
  if (ctx->dirty) {
    ctx->dirty = 0;
    caml_scan_stack(f, stack);
  }
}

void caml_scan_stack(scanning_action f, value stack)
{
  value *low, *high, *sp;
  Assert(Is_block(stack) && Tag_val(stack) == Stack_tag);

  if (Is_promoted_hd(Hd_val(stack)))
    Assert(!Is_young(stack)); //FIXME

  high = (value*)stack + Wosize_val(stack);
  low = high + Stack_ctx(stack)->sp;
  for (sp = low; sp < high; sp++) {
    f(*sp, sp);
  }
}

void caml_do_fiber_roots(scanning_action f, struct caml_runqueue* rq)
{
  f(rq->current, &rq->current);
  deque_iter(f, &rq->front);
  deque_iter(f, &rq->back);
}

/*
  MVars.

  Like Haskell :)
*/

typedef enum {
  MVAR_LOCKED = 0,
  MVAR_EMPTY,
  MVAR_TAKE_WAITING,
  MVAR_FULL,
  MVAR_PUT_WAITING
} mvar_state;

enum {
  MVAR_STATE,
  MVAR_WAITQ_HEAD,
  MVAR_WAITQ_TAIL,
  MVAR_VALUE,

  MVAR_NFIELDS
};

static mvar_state lock_mvar(value m)
{
  mvar_state state;
  while (1) {
    value s = Field(m, MVAR_STATE);
    if (s != Val_int(MVAR_LOCKED) &&
        caml_atomic_cas_field(m, MVAR_STATE, s, Val_int(MVAR_LOCKED))) {
      state = Int_val(s);
      break;
    }
    caml_domain_spin();
  } 

  /* check invariants */
  if (state == MVAR_EMPTY || state == MVAR_FULL) {
    Assert(Field(m, MVAR_WAITQ_HEAD) == Val_unit &&
           Field(m, MVAR_WAITQ_TAIL) == Val_unit);
  } else {
    Assert(Field(m, MVAR_WAITQ_HEAD) != Val_unit &&
           Field(m, MVAR_WAITQ_TAIL) != Val_unit);
    Assert(Field(Field(m, MVAR_WAITQ_TAIL), FIBER_NEXT) == Val_unit);
  }

  return state;
}

static void unlock_mvar(value m, mvar_state state)
{
  Assert(state != MVAR_LOCKED &&
         Field(m, MVAR_STATE) == Val_int(MVAR_LOCKED));
  //int x = caml_atomic_cas_field(m, MVAR_STATE, Val_int(MVAR_LOCKED), Val_int(state));
  //Assert(x);
  caml_modify_field(m, MVAR_STATE, Val_int(state));
}

CAMLprim value caml_mvar_new(value unit)
{
  CAMLparam1(unit);
  CAMLlocal1(ret);
  ret = caml_alloc(MVAR_NFIELDS, 0);
  caml_initialize_field(ret, MVAR_STATE, Val_int(MVAR_EMPTY));
  caml_initialize_field(ret, MVAR_WAITQ_HEAD, Val_unit);
  caml_initialize_field(ret, MVAR_WAITQ_TAIL, Val_unit);
  caml_initialize_field(ret, MVAR_VALUE, Val_unit);
  CAMLreturn (ret);
}

static value suspend_on_mvar(value m, value block, int is_first)
{
  value self = save_context(block);
  caml_gc_log("Suspending on mvar");
  if (is_first) {
    /* first fiber on waitq, update head and tail pointers */
    caml_modify_field(m, MVAR_WAITQ_HEAD, self);
    caml_modify_field(m, MVAR_WAITQ_TAIL, self);
  } else {
    /* joining a queue, just update tail */
    caml_modify_field(Field(m, MVAR_WAITQ_TAIL), FIBER_NEXT, self);
    caml_modify_field(m, MVAR_WAITQ_TAIL, self);
  }
  dirty_stack(Field(self, FIBER_STACK));
  return load_context(next_fiber());
}

static value wake_fiber(value fib, value ret)
{
  CAMLparam2(fib, ret);
  struct domain* owner = Stack_ctx(Field(fib, FIBER_STACK))->domain;
  if (owner == caml_domain_self()) {
    /* Which fiber should we run next?
       Here we never switch. This may not be optimal. */
    caml_gc_log("Waking fiber locally");
   
    append(&caml_runqueue->back, fib);
    CAMLreturn (ret);
    
    /* Alternative: 
       append(&caml_runqueue->back, save_context(ret));
       CAMLreturn (load_context(fib)); */
  } else {
    struct caml_runqueue* rq = owner->runqueue;
    caml_gc_log("Waking fiber remotely");
    With_mutex(&rq->woken_lock) {
      caml_modify_field(fib, FIBER_NEXT, caml_read_root(rq->woken));
      caml_modify_root(rq->woken, fib);
    }
    CAMLreturn (ret);
  }
}

CAMLprim value caml_mvar_put(value mv)
{
  value m = Field(mv, 0);
  value v = Field(mv, 1);
  mvar_state state = lock_mvar(m);
  value ret = Val_unit;
  
  if (state == MVAR_EMPTY) {
    caml_modify_field(m, MVAR_VALUE, v);
    state = MVAR_FULL;
  } else if (state == MVAR_TAKE_WAITING) {
    value fib = Field(m, MVAR_WAITQ_HEAD);
    value next = Field(fib, FIBER_NEXT);
    caml_modify_field(m, MVAR_WAITQ_HEAD, next);
    caml_modify_field(fib, FIBER_NEXT, Val_unit);
    if (next == Val_unit) {
      /* last fiber in the queue */
      Assert(Field(m, MVAR_WAITQ_TAIL) == fib);
      caml_modify_field(m, MVAR_WAITQ_TAIL, Val_unit);
      state = MVAR_EMPTY;
    }

    caml_modify_field(fib, FIBER_BLOCKVAL, v);
    ret = wake_fiber(fib, Val_unit);
  } else {
    ret = suspend_on_mvar(m, v, state == MVAR_FULL);
    state = MVAR_PUT_WAITING;
  }

  unlock_mvar(m, state);
  return ret;
}

CAMLprim value caml_mvar_take(value m)
{
  mvar_state state = lock_mvar(m);
  value ret;
  if (state == MVAR_FULL) {
    ret = Field(m, MVAR_VALUE);
    caml_modify_field(m, MVAR_VALUE, Val_unit);
    state = MVAR_EMPTY;
  } else if (state == MVAR_PUT_WAITING) {
    value fib = Field(m, MVAR_WAITQ_HEAD);
    value next = Field(fib, FIBER_NEXT);
    caml_modify_field(m, MVAR_WAITQ_HEAD, next);
    caml_modify_field(fib, FIBER_NEXT, Val_unit);
    if (next == Val_unit) {
      /* last fiber in the queue */
      Assert(Field(m, MVAR_WAITQ_TAIL) == fib);
      caml_modify_field(m, MVAR_WAITQ_TAIL, Val_unit);
      state = MVAR_FULL;
    }

    ret = Field(m, MVAR_VALUE);
    caml_modify_field(m, MVAR_VALUE, Field(fib, FIBER_BLOCKVAL));
    caml_modify_field(fib, FIBER_BLOCKVAL, Val_unit);

    ret = wake_fiber(fib, ret);
  } else {
    ret = suspend_on_mvar(m, Val_unit, state == MVAR_EMPTY);
    state = MVAR_TAKE_WAITING;
  }

  unlock_mvar(m, state);
  return ret;
}
