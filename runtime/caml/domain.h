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

#include "camlatomic.h"
#include "config.h"
#include "mlvalues.h"
#include "domain_state.h"
#include "platform.h"

/* The runtime currently has a hard limit on the number of domains.
   This hard limit may go away in the future. */
#ifdef ARCH_SIXTYFOUR
#define Max_domains 128
#else
#define Max_domains 16
#endif

/* is the minor heap full or an external interrupt has been triggered */
Caml_inline int caml_check_gc_interrupt(caml_domain_state * dom_st)
{
  CAMLalloc_point_here;
  uintnat young_limit = atomic_load_relaxed(&dom_st->young_limit);
  if ((uintnat)dom_st->young_ptr < young_limit) {
    /* Synchronise for the case when [young_limit] was used to interrupt
       us. */
    atomic_thread_fence(memory_order_acquire);
    return 1;
  }
  return 0;
}

#define Caml_check_gc_interrupt(dom_st)           \
  (CAMLunlikely(caml_check_gc_interrupt(dom_st)))

asize_t caml_norm_minor_heap_size (intnat);
int caml_reallocate_minor_heap(asize_t);
void caml_update_minor_heap_max(uintnat minor_heap_wsz);

/* is there a STW interrupt queued that needs servicing */
int caml_incoming_interrupts_queued(void);

void caml_poll_gc_work(void);
void caml_handle_gc_interrupt(void);
void caml_handle_incoming_interrupts(void);

CAMLextern void caml_interrupt_self(void);
void caml_reset_young_limit(caml_domain_state *);

CAMLextern void caml_reset_domain_lock(void);
CAMLextern int caml_bt_is_in_blocking_section(void);
CAMLextern int caml_bt_is_self(void);
CAMLextern intnat caml_domain_is_multicore (void);
CAMLextern void caml_bt_enter_ocaml(void);
CAMLextern void caml_bt_exit_ocaml(void);
CAMLextern void caml_acquire_domain_lock(void);
CAMLextern void caml_release_domain_lock(void);

/* These hooks are not modified after other domains are spawned. */
CAMLextern void (*caml_atfork_hook)(void);
CAMLextern void (*caml_domain_initialize_hook)(void);
CAMLextern void (*caml_domain_stop_hook)(void);
CAMLextern void (*caml_domain_external_interrupt_hook)(void);

CAMLextern void caml_init_domains(uintnat minor_heap_wsz);
CAMLextern void caml_init_domain_self(int);

CAMLextern uintnat caml_minor_heap_max_wsz;

CAMLextern atomic_uintnat caml_num_domains_running;

Caml_inline intnat caml_domain_alone(void)
{
  return atomic_load_acq(&caml_num_domains_running) == 1;
}

#ifdef DEBUG
int caml_domain_is_in_stw(void);
#endif

int caml_try_run_on_all_domains_with_spin_work(
  void (*handler)(caml_domain_state*, void*, int, caml_domain_state**),
  void* data,
  void (*leader_setup)(caml_domain_state*),
  void (*enter_spin_callback)(caml_domain_state*, void*),
  void* enter_spin_data);
int caml_try_run_on_all_domains(
  void (*handler)(caml_domain_state*, void*, int, caml_domain_state**),
  void*,
  void (*leader_setup)(caml_domain_state*));

/* barriers */
typedef uintnat barrier_status;
void caml_global_barrier(void);
barrier_status caml_global_barrier_begin(void);
int caml_global_barrier_is_final(barrier_status);
void caml_global_barrier_end(barrier_status);
int caml_global_barrier_num_domains(void);

int caml_domain_is_terminating(void);

#endif /* CAML_INTERNALS */

#ifdef __cplusplus
}
#endif

#endif /* CAML_DOMAIN_H */
