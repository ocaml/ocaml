#ifndef CAML_FIBER_H
#define CAML_FIBER_H

#include "misc.h"
#include "mlvalues.h"
#include "memory.h"
#include "roots.h"


/* One word at the base of the stack is used to store the stack pointer */
#define Stack_ctx_words 6
#define Stack_base(stk) (Op_val(stk) + Stack_ctx_words)
#define Stack_high(stk) (Op_val(stk) + Wosize_val(stk))
#define Stack_sp(stk) (*(long*)(Op_val(stk) + 0))
#define Stack_dirty_domain(stk) (*(struct domain**)(Op_val(stk) + 1))
#define Stack_handle_value(stk) (*(Op_val(stk) + 2))
#define Stack_handle_exception(stk) (*(Op_val(stk) + 3))
#define Stack_handle_effect(stk) (*(Op_val(stk) + 4))
#define Stack_parent(stk) (*(Op_val(stk) + 5))

value caml_find_performer(value stack);

/* The table of global identifiers */
extern caml_root caml_global_data;

#define Trap_pc(tp) ((tp)[0])
#define Trap_link(tp) ((tp)[1])

value caml_alloc_main_stack (uintnat init_size);
void caml_init_main_stack(void);
void caml_scan_dirty_stack(scanning_action, value stack);
void caml_scan_dirty_stack_domain(scanning_action f, value stack,
                                  struct domain* domain);
void caml_scan_stack(scanning_action, value stack);
void caml_save_stack_gc();
void caml_restore_stack_gc();
void caml_restore_stack();
void caml_clean_stack(value stack);
void caml_clean_stack_domain(value stack, struct domain* domain);
void caml_realloc_stack (asize_t required_size, value* save, int nsave);
void caml_change_max_stack_size (uintnat new_max_size);
int  caml_on_current_stack(value*);
int  caml_running_main_fiber();
value caml_switch_stack(value stk);
value caml_fiber_death();

#ifdef NATIVE_CODE
void caml_get_stack_sp_pc (value stack, char** sp /* out */, uintnat* pc /* out */);
#endif

#endif /* CAML_FIBER_H */
