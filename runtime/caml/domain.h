/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*      KC Sivaramakrishnan, Indian Institute of Technology, Madras       */
/*                   Stephen Dolan, University of Cambridge               */
/*                                                                        */
/*   Copyright 2019 Indian Institute of Technology, Madras                */
/*   Copyright 2019 University of Cambridge                               */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#ifndef CAML_DOMAIN_H
#define CAML_DOMAIN_H

#ifdef __cplusplus
extern "C" {
#endif

#ifdef CAML_INTERNALS

#include "config.h"
#include "mlvalues.h"
#include "domain_state.h"
#include "memory.h"
#include "major_gc.h"
#include "platform.h"

struct domain {
  struct dom_internal* internals;
  caml_domain_state* state;
};

#define Caml_check_gc_interrupt(dom_st)   \
  (CAMLalloc_point_here, \
   CAMLunlikely((uintnat)(dom_st)->young_ptr < (dom_st)->young_limit))

asize_t caml_norm_minor_heap_size (intnat);
int caml_reallocate_minor_heap(asize_t);

int caml_incoming_interrupts_queued(void);

void caml_handle_gc_interrupt(void);

void caml_handle_incoming_interrupts(void);

void caml_request_major_slice (void);

void caml_request_minor_gc (void);

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

CAMLexport void caml_reset_domain_lock(void);
CAMLexport int caml_bt_is_in_blocking_section(void);
CAMLexport intnat caml_domain_is_multicore (void);
CAMLexport void caml_bt_enter_ocaml(void);
CAMLexport void caml_bt_exit_ocaml(void);
CAMLexport void caml_acquire_domain_lock(void);
CAMLexport void caml_release_domain_lock(void);

CAMLextern void caml_enter_blocking_section(void);
CAMLextern void caml_leave_blocking_section(void);

CAMLextern void (*caml_enter_blocking_section_hook)(void);
CAMLextern void (*caml_leave_blocking_section_hook)(void);

CAMLextern void (*caml_atfork_hook)(void);

CAMLextern void (*caml_domain_start_hook)(void);
CAMLextern void (*caml_domain_stop_hook)(void);

void caml_init_domains(uintnat minor_heap_size);
void caml_init_domain_self(int);

struct domain* caml_domain_self();
struct domain* caml_owner_of_young_block(value);
struct domain* caml_domain_of_id(int);

CAMLextern atomic_uintnat caml_num_domains_running;
CAMLextern uintnat caml_minor_heaps_base;
CAMLextern uintnat caml_minor_heaps_end;

INLINE intnat caml_domain_alone()
{
  return atomic_load_acq(&caml_num_domains_running) == 1;
}

typedef struct interrupt interrupt;
typedef void (*domain_rpc_handler)(struct domain*, void*, interrupt*);

CAMLcheckresult
int caml_domain_rpc(struct domain*,
                     domain_rpc_handler, void*);

typedef struct interrupt interrupt;
void caml_acknowledge_interrupt(interrupt*);

#ifdef DEBUG
int caml_domain_is_in_stw();
#endif

void caml_run_on_all_running_domains_during_stw(void (*handler)(struct domain*, void*), void* data);
int caml_try_run_on_all_domains_with_spin_work(
  void (*handler)(struct domain*, void*, int, struct domain**), void* data,
  void (*leader_setup)(struct domain*),
  void (*enter_spin_callback)(struct domain*, void*), void* enter_spin_data);
int caml_try_run_on_all_domains(void (*handler)(struct domain*, void*, int, struct domain**), void*, void (*leader_setup)(struct domain*));

void caml_global_barrier();

typedef uintnat barrier_status;
barrier_status caml_global_barrier_begin();
int caml_global_barrier_is_final(barrier_status);
void caml_global_barrier_end(barrier_status);
int caml_global_barrier_num_domains();
int caml_domain_is_terminating(void);

#endif /* CAML_INTERNALS */

#ifdef __cplusplus
}
#endif

#endif /* CAML_DOMAIN_H */
