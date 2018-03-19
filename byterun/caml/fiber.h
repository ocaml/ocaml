#ifndef CAML_FIBER_H
#define CAML_FIBER_H

#ifdef CAML_INTERNALS

#include "misc.h"
#include "mlvalues.h"
#include "memory.h"
#include "roots.h"


/* One word at the base of the stack is used to store the stack pointer */
#define Stack_ctx_words 6
#define Stack_base(stk) (Op_val(stk) + Stack_ctx_words)

/* 16-byte align-down caml_stack_high for certain architectures like arm64
 * demand 16-byte alignment. Leaves a word unused at the bottom of the stack if
 * the Op_val(stk) + Wosize_val(stk) is not 16-byte aligned. */
#define Stack_high(stk) ((value*)(((value)(Op_val(stk) + Wosize_val(stk))) & (-1uLL << 4)))

#define Stack_sp(stk) (*(long*)(Op_val(stk) + 0))
#define Stack_dirty_domain(stk) (*(struct domain**)(Op_val(stk) + 1))
#define Stack_handle_value(stk) (*(Op_val(stk) + 2))
#define Stack_handle_exception(stk) (*(Op_val(stk) + 3))
#define Stack_handle_effect(stk) (*(Op_val(stk) + 4))
#define Stack_parent_offset 5
#define Stack_parent(stk) (*(Op_val(stk) + Stack_parent_offset))

/* States for Stack_dirty_domain field */
/* A clean fiber does not have pointers into any minor heaps */
#define FIBER_CLEAN ((struct domain*)0)
/* A clean fiber is being scanned by a GC thread */
#define FIBER_SCANNING ((struct domain*)1)

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

value caml_find_performer(value stack);

/* The table of global identifiers */
extern caml_root caml_global_data;

#define Trap_pc(tp) ((tp)[0])
#define Trap_link(tp) ((tp)[1])

value caml_alloc_main_stack (uintnat init_size);
void caml_init_main_stack(void);
void caml_scan_dirty_stack_domain(scanning_action f, void*, value stack,
                                  struct domain* domain);
void caml_scan_stack(scanning_action, void*, value stack);
void caml_save_stack_gc();
void caml_restore_stack_gc();
void caml_restore_stack();
void caml_clean_stack(value stack);
void caml_clean_stack_domain(value stack, struct domain* domain);
void caml_realloc_stack (asize_t required_size, value* save, int nsave);
void caml_change_max_stack_size (uintnat new_max_size);
int  caml_on_current_stack(value*);
int  caml_running_main_fiber();
#ifdef NATIVE_CODE
int caml_switch_stack(value stk);
#else
value caml_switch_stack(value stk);
#endif
value caml_fiber_death();
void caml_darken_stack(value stack);
value caml_reverse_fiber_stack(value stack);

#ifdef NATIVE_CODE
void caml_get_stack_sp_pc (value stack, char** sp /* out */, uintnat* pc /* out */);
#endif

#endif /* CAML_INTERNALS */

#endif /* CAML_FIBER_H */
