#ifndef CAML_DOMAIN_H
#define CAML_DOMAIN_H

#ifdef CAML_INTERNALS

#include "config.h"
#include "mlvalues.h"
#include "domain_state.h"
#include "memory.h"
#include "major_gc.h"

struct domain {
  struct dom_internal* internals;
  caml_domain_state* state;
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

void caml_handle_incoming_interrupts(void);

void caml_urge_major_slice (void);

void caml_interrupt_self(void);

#if defined(COLLECT_STATS) && defined(NATIVE_CODE)
struct detailed_stats {
  uint64 allocations;

  uint64 mutable_loads;
  uint64 immutable_loads;

  uint64 mutable_stores;
  uint64 immutable_stores;

  uint64 extcall_noalloc;
  uint64 extcall_alloc;
  uint64 extcall_alloc_stackargs;

  uint64 tailcall_imm;
  uint64 tailcall_ind;
  uint64 call_imm;
  uint64 call_ind;

  uint64 stackoverflow_checks;
};
#endif

void caml_sample_gc_stats(struct gc_stats* buf);
void caml_print_stats(void);

CAMLextern void caml_enter_blocking_section(void);
CAMLextern void caml_leave_blocking_section(void);

CAMLextern void (*caml_enter_blocking_section_hook)(void);
CAMLextern void (*caml_leave_blocking_section_hook)(void);

void caml_init_domains(uintnat minor_heap_size);
void caml_init_domain_self(int);

struct domain* caml_domain_self();
struct domain* caml_owner_of_young_block(value);
struct domain* caml_domain_of_id(int);

typedef struct interrupt interrupt;
typedef void (*domain_rpc_handler)(struct domain*, void*, interrupt*);

int caml_domain_alone();

CAMLcheckresult
int caml_domain_rpc(struct domain*,
                     domain_rpc_handler, void*);

typedef struct interrupt interrupt;
void caml_acknowledge_interrupt(interrupt*);

int caml_try_run_on_all_domains(void (*callback)(struct domain*, void*), void*);

void caml_global_barrier();

typedef uintnat barrier_status;
barrier_status caml_global_barrier_begin();
int caml_global_barrier_is_final(barrier_status);
void caml_global_barrier_end(barrier_status);
int caml_global_barrier_num_domains();

#endif /* CAML_INTERNALS */

#endif /* CAML_DOMAIN_H */
