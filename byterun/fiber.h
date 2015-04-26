#ifndef CAML_FIBER_H
#define CAML_FIBER_H

#include "misc.h"
#include "mlvalues.h"
#include "memory.h"
#include "roots.h"


CAMLextern __thread value caml_current_stack;
CAMLextern __thread value caml_parent_stack;
CAMLextern __thread value * caml_stack_high;
CAMLextern __thread value * caml_stack_threshold;
CAMLextern __thread value * caml_extern_sp;
CAMLextern __thread intnat caml_trap_sp_off;
CAMLextern __thread intnat caml_trap_barrier_off;
CAMLextern __thread intnat caml_extra_args;
CAMLextern __thread int caml_c_call_args;
CAMLextern __thread code_t caml_saved_pc;

value caml_handle(value body, value hval, value heff, value hexn, intnat extra_args);
value caml_perform(value effect);
value caml_continue(value cont, value ret, intnat extra_args);
value caml_finish(value ret);
value caml_finish_exception(value exn);

struct caml_runqueue;

void caml_scan_dirty_stack(scanning_action, value stack);
void caml_scan_stack(scanning_action, value stack);
void caml_save_stack_gc();
void caml_restore_stack_gc();
void caml_clean_stack(value stack);


/* The table of global identifiers */
extern caml_root caml_global_data;

#define Trap_pc(tp) ((tp)[0])
#define Trap_link(tp) ((tp)[1])



void caml_realloc_stack (asize_t required_size, value* save, int nsave);
void caml_change_max_stack_size (uintnat new_max_size);
int caml_on_current_stack(value*);

struct caml_runqueue* caml_init_runqueue();
void caml_init_fibers();

int caml_running_main_fiber();
value caml_fiber_death();

#endif /* CAML_FIBER_H */
