#ifndef CAML_DOMAIN_H
#define CAML_DOMAIN_H

#include "mlvalues.h"
CAMLextern __thread atomic_uintnat caml_young_limit;

struct domain {
  int id;
  int is_main;
  uintnat initial_minor_heap_size;

  struct dom_internal* internals;
  struct caml_runqueue* runqueue;
  struct caml_heap_state* shared_heap;
  struct caml_sampled_roots* sampled_roots;
  struct caml_remembered_set* remembered_set;

  struct caml__roots_block** local_roots;
  char** young_ptr;
  char** young_end;
  value** mark_stack;
  int* mark_stack_count;
};


/* FIXME atomic access below */
#define Caml_get_young_limit() caml_young_limit.val

#define Caml_check_gc_interrupt(p) ((uintnat)(p) < Caml_get_young_limit())

void caml_update_young_limit(uintnat);

void caml_handle_gc_interrupt(int required_words);

void caml_trigger_stw_gc(void);

CAMLextern void caml_enter_blocking_section(void);
CAMLextern void caml_leave_blocking_section(void);

void caml_domain_register_main(uintnat minor_heap_size);


struct domain* caml_domain_self();

typedef void (*domain_rpc_handler)(struct domain*, void*);

void caml_domain_rpc(struct domain*, 
                     domain_rpc_handler, void*);

void caml_domain_spin();

#endif /* CAML_DOMAIN_H */
