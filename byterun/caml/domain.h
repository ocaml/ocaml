#ifndef CAML_DOMAIN_H
#define CAML_DOMAIN_H

#include "mlvalues.h"
#include "domain_state.h"
#include "memory.h"

struct domain {
  int id;
  int vm_inited;

  struct dom_internal* internals;
  struct caml_heap_state* shared_heap;
  struct caml__roots_block** local_roots;
  struct caml_domain_state* state;
  value** mark_stack;
  uintnat* mark_stack_count;
};

#ifdef __GNUC__
  #define Caml_check_gc_interrupt(dom_st) \
    __builtin_expect(((uintnat)(dom_st)->young_ptr < (dom_st)->young_limit), 0)
#else
  #define Caml_check_gc_interrupt(dom_st) \
    ((uintnat)(dom_st)->young_ptr < (dom_st)->young_limit)
#endif
asize_t caml_norm_minor_heap_size (intnat);
void caml_reallocate_minor_heap(asize_t);

void caml_handle_gc_interrupt(void);

void caml_trigger_stw_gc(void);

void caml_urge_major_slice (void);

void caml_interrupt_self(void);


CAMLextern void caml_enter_blocking_section(void);
CAMLextern void caml_leave_blocking_section(void);

CAMLextern void (*caml_enter_blocking_section_hook)(void);
CAMLextern void (*caml_leave_blocking_section_hook)(void);

void caml_init_domains(uintnat minor_heap_size);
void caml_init_domain_self(int);


struct domain* caml_domain_self();

typedef void (*domain_rpc_handler)(struct domain*, void*);

struct domain* caml_random_domain();

struct domain* caml_owner_of_young_block(value);

struct domain* caml_domain_of_id(int);

void caml_domain_rpc(struct domain*,
                     domain_rpc_handler, void*);

#endif /* CAML_DOMAIN_H */
