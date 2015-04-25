#ifndef CAML_DOMAIN_H
#define CAML_DOMAIN_H

#include "mlvalues.h"
CAMLextern __thread atomic_uintnat caml_young_limit;

struct domain {
  int id;
  int is_main;

  struct dom_internal* internals;
  struct caml_runqueue* runqueue;
  struct caml_heap_state* shared_heap;
  struct caml_remembered_set* remembered_set;

  struct caml__roots_block** local_roots;
#ifdef NATIVE_CODE
  /* FIXME: represent current stack here */
#else
  value* current_stack;
  value* parent_stack;
#endif

  char** young_ptr;
  char** young_end;
  value** mark_stack;
  int* mark_stack_count;
};


/* FIXME atomic access below */
#define Caml_get_young_limit() caml_young_limit.val

#define Caml_check_gc_interrupt(p) ((uintnat)(p) < Caml_get_young_limit())

asize_t caml_norm_minor_heap_size (intnat);
void caml_reallocate_minor_heap(asize_t);

void caml_update_young_limit(uintnat);

void caml_handle_gc_interrupt(int required_words);

void caml_trigger_stw_gc(void);

void caml_urge_major_slice (void);

void caml_interrupt_self(void);


CAMLextern void caml_enter_blocking_section(void);
CAMLextern void caml_leave_blocking_section(void);

void caml_init_domains(uintnat minor_heap_size);


struct domain* caml_domain_self();

typedef void (*domain_rpc_handler)(struct domain*, void*);

struct domain* caml_random_domain();

struct domain* caml_owner_of_young_block(value);

struct domain* caml_domain_of_id(int);

void caml_domain_rpc(struct domain*,
                     domain_rpc_handler, void*);

#endif /* CAML_DOMAIN_H */
