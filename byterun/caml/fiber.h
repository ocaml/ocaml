#ifndef CAML_FIBER_H
#define CAML_FIBER_H

#ifdef CAML_INTERNALS

#include "misc.h"
#include "mlvalues.h"
#include "memory.h"
#include "roots.h"

struct stack_info {
#ifdef NATIVE_CODE
  void* sp;
#else
  value* sp;
#endif
  value handle_value;
  value handle_exn;
  value handle_effect;
  struct stack_info* parent;
  uintnat wosize;
  uintnat magic;
};

/* One word at the base of the stack is used to store the stack pointer */
#define Stack_ctx_words (sizeof(struct stack_info) / sizeof(value))
#define Stack_base(stk) ((value*)(stk + 1))

/* 16-byte align-down caml_stack_high for certain architectures like arm64
 * demand 16-byte alignment. Leaves a word unused at the bottom of the stack if
 * the Op_val(stk) + Wosize_val(stk) is not 16-byte aligned. */
#define Stack_high(stk) ((value*)(((uintnat)stk + sizeof(value) * stk->wosize) & (-1uLL << 4)))

#define Stack_handle_value(stk) (stk)->handle_value
#define Stack_handle_exception(stk) (stk)->handle_exn
#define Stack_handle_effect(stk) (stk)->handle_effect
#define Stack_parent_offset 5
#define Stack_parent(stk) (stk)->parent

#ifdef NATIVE_CODE

/* Stack layout for native code. Stack grows downwards.
 *
 * +------------------------+ <--- Stack_high
 * | caml_fiber_exn_handler |
 * +------------------------+
 * |    debugger_slot       |
 * +------------------------+
 * | caml_fiber_val_handler |
 * +------------------------+
 * |      caml_handler      |
 * +------------------------+
 * |                        |
 * .      OCaml frames      . <--- sp
 * |                        |
 * +------------------------+ <--- Stack_threshold
 * |                        |
 * .      Slop space        .
 * |                        |
 * +------------------------+ <--- Stack_base
 * |        parent          |
 * +------------------------+
 * |  handle effect closure |
 * +------------------------+
 * |   handle exn closure   |
 * +------------------------+
 * |   handle val closure   |
 * +------------------------+
 * |      dirty domain      |
 * +------------------------+
 * | Offset of sp from high |
 * +------------------------+ <--- stack object
 * |      HEADER WORD       |
 * +------------------------+
 */


/* Slot for debugger. This is the previous trap frame offset of the outermost
 * trap frame (which by definition does not have a previous trap frame). The
 * value in this slot is the offset (in bytes) to Stack_parent slot, which is
 * accessed in DWARF. */
#define Stack_debugger_slot(stk) (*(value*)(Stack_high(stk) - 2))
#define Stack_debugger_slot_offset_to_parent_slot(stk) \
  (sizeof(value) * ((Stack_high(stk) - (value*)stk) - 2 - Stack_parent_offset))
#endif

/* The table of global identifiers */
extern caml_root caml_global_data;

#define Trap_pc(tp) ((tp)[0])
#define Trap_link(tp) ((tp)[1])

struct stack_info* caml_alloc_main_stack (uintnat init_size);
void caml_init_main_stack(void);
void caml_scan_stack(scanning_action f, void* fdata, struct stack_info* stack);
void caml_restore_stack();
/* try to grow the stack until at least required_size words are available.
   returns nonzero on success */
int caml_try_realloc_stack (asize_t required_size);
void caml_change_max_stack_size (uintnat new_max_size);
void caml_maybe_expand_stack();
void caml_free_stack(struct stack_info* stk);
int  caml_on_current_stack(value*);
#ifdef NATIVE_CODE
int caml_switch_stack(struct stack_info* stk, int should_free);
#else
struct stack_info* caml_switch_stack(struct stack_info* stk);
#endif

#ifdef NATIVE_CODE
void caml_get_stack_sp_pc (struct stack_info* stack, char** sp /* out */, uintnat* pc /* out */);
#endif

value caml_continuation_use (value cont);

/* Replace the stack of a continuation that was previouly removed
   with caml_continuation_use. The GC must not be allowed to run
   between continuation_use and continuation_replace.
   Used for cloning continuations and continuation backtraces. */
void caml_continuation_replace(value cont, struct stack_info* stack);

#endif /* CAML_INTERNALS */

#endif /* CAML_FIBER_H */
