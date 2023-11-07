/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*            Jacques-Henri Jourdan, projet Gallium, INRIA Paris          */
/*                                                                        */
/*   Copyright 2016 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#define CAML_INTERNALS

#include <stdbool.h>
#include "caml/memory.h"
#include "caml/memprof.h"

/* type aliases for the hierarchy of structures for managing memprof status. */

typedef struct memprof_domain_s memprof_domain_s, *memprof_domain_t;
typedef struct memprof_thread_s memprof_thread_s, *memprof_thread_t;

/* Per-thread memprof state. */

struct memprof_thread_s {
  /* [suspended] is used for inhibiting memprof callbacks when
     a callback is running or when an uncaught exception handler is
     called. */
  bool suspended;

  /* TODO: More fields to add here */

  /* Per-domain memprof information */
  memprof_domain_t domain;

  /* Linked list of thread structures for this domain. Could use a
   * doubly-linked list for performance, but I haven't measured it. */
  memprof_thread_t next;
};

/* A memprof configuration is held in an object on the Caml
 * heap. These are getter macros for each field. */

#define Stopped(config)          Bool_val(Field(config, 0))
#define Running(config)          ((config != Val_unit) && !Stopped(config))
#define Lambda(config)           Double_val(Field(config, 1))
#define One_log1m_lambda(config) Double_val(Field(config, 2))
#define Callstack_size(config)   Int_val(Field(config, 3)
#define Alloc_minor(config)      Field(config, 4)
#define Alloc_major(config)      Field(config, 5)
#define Promote(config)          Field(config, 6)
#define Dealloc_minor(config)    Field(config, 7)
#define Dealloc_major(config)    Field(config, 8)

/* The 'stopped' field is the only one we ever update. */

#define Set_stopped(config, flag) (Field(config, 0) = Val_bool(flag))

/* Per-domain memprof state */

struct memprof_domain_s {
  /* The owning domain */
  caml_domain_state *caml_state;

  /* Linked list of threads in this domain */
  memprof_thread_t threads;

  /* The current thread's memprof state. Note that there may not be a
     "current thread". TODO: maybe this shouldn't be nullable.
     Nullability costs us some effort and may be meaningless. See call
     site of caml_memprof_leave_thread() in st_stubs.c. */
  memprof_thread_t current;

  /* TODO: More fields to add here */

  /* The current profiling configuration for this domain. */
  value config;
};

/**** Create and destroy thread state structures ****/

static memprof_thread_t thread_create(memprof_domain_t domain)
{
  memprof_thread_t thread = caml_stat_alloc(sizeof(memprof_thread_s));
  if (!thread) {
    return NULL;
  }
  thread->suspended = false;

  /* attach to domain record */
  thread->domain = domain;
  thread->next = domain->threads;
  domain->threads = thread;

  return thread;
}

static void thread_destroy(memprof_thread_t thread)
{
  memprof_domain_t domain = thread->domain;

  if (domain->current == thread) {
    domain->current = NULL;
  }
  /* remove thread from the per-domain list. Could go faster if we
   * used a doubly-linked list, but that's premature optimisation
   * at this point. */
  memprof_thread_t *p = &domain->threads;
  while (*p != thread) {
    p = &(*p)->next;
  }

  *p = thread->next;

  caml_stat_free(thread);
}

/**** Create and destroy domain state structures ****/

static void domain_destroy(memprof_domain_t domain)
{
  memprof_thread_t thread = domain->threads;
  while (thread) {
    memprof_thread_t next = thread->next;
    thread_destroy(thread);
    thread = next;
  }

  caml_stat_free(domain);
}

static memprof_domain_t domain_create(caml_domain_state *caml_state)
{
  memprof_domain_t domain = caml_stat_alloc(sizeof(memprof_domain_s));
  if (!domain) {
    return NULL;
  }

  domain->caml_state = caml_state;
  domain->threads = NULL;
  domain->current = NULL;
  domain->config = Val_unit;

  /* create initial thread for domain */
  memprof_thread_t thread = thread_create(domain);
  if (thread) {
    domain->current = thread;
  } else {
    domain_destroy(domain);
    domain = NULL;
  }
  return domain;
}

/**** Interface to domain module ***/

void caml_memprof_new_domain(caml_domain_state *parent,
                             caml_domain_state *child)
{
  memprof_domain_t domain = domain_create(child);

  child->memprof = domain;
  /* domain inherits configuration from parent */
  if (domain && parent) {
    domain->config = parent->memprof->config;
  }
}

void caml_memprof_delete_domain(caml_domain_state *state)
{
  if (!state->memprof) {
    return;
  }
  domain_destroy(state->memprof);
  state->memprof = NULL;
}

/**** Interface with domain action-pending flag ****/

/* If profiling is active in the current domain, and we may have some
 * callbacks pending, set the action pending flag. */

static void set_action_pending_as_needed(memprof_domain_t domain)
{
  /* if (condition) caml_set_action_pending(domain->caml_state); */
}

/* Set the suspended flag on `domain` to `s`. */

static void update_suspended(memprof_domain_t domain, bool s)
{
  if (domain->current) {
    domain->current->suspended = s;
  }
  caml_memprof_renew_minor_sample(domain->caml_state);
  if (!s) set_action_pending_as_needed(domain);
}

/* Set the suspended flag on the current domain to `s`. */

void caml_memprof_update_suspended(bool s) {
  update_suspended(Caml_state->memprof, s);
}

/**** Sampling procedures ****/

Caml_inline bool running(memprof_domain_t domain)
{
  memprof_thread_t thread = domain->current;

  if (thread && !thread->suspended) {
    value config = domain->config;
    return Running(config);
  }
  return false;
}

/* Renew the next sample in a domain's minor heap. Could race with
 * sampling and profile-stopping code, so do not call from another
 * domain unless the world is stopped. Must be called after each minor
 * sample and after each minor collection. In practice, this is called
 * at each minor sample, at each minor collection, and when sampling
 * is suspended and unsuspended. Extra calls do not change the
 * statistical properties of the sampling because of the
 * memorylessness of the geometric distribution. */

void caml_memprof_renew_minor_sample(caml_domain_state *state)
{
  memprof_domain_t domain = state->memprof;
  value *trigger = state->young_start;
  if (running(domain)) {
    /* set trigger based on geometric distribution */
  }
  CAMLassert((trigger >= state->young_start) &&
             (trigger <= state->young_ptr));
  state->memprof_young_trigger = trigger;
  caml_reset_young_limit(state);
}

/**** Interface with systhread. ****/

CAMLexport memprof_thread_t caml_memprof_new_thread(caml_domain_state *state)
{
  return thread_create(state->memprof);
}

CAMLexport memprof_thread_t caml_memprof_main_thread(caml_domain_state *state)
{
  memprof_domain_t domain = state->memprof;
  memprof_thread_t thread = domain->threads;

  /* There should currently be just one thread in this domain */
  CAMLassert(thread);
  CAMLassert(thread->next == NULL);
  return thread;
}

CAMLexport void caml_memprof_delete_thread(memprof_thread_t thread)
{
  thread_destroy(thread);
}

CAMLexport void caml_memprof_enter_thread(memprof_thread_t thread)
{
  thread->domain->current = thread;
  update_suspended(thread->domain, thread->suspended);
}

/**** Interface to OCaml ****/

#include "caml/fail.h"

CAMLprim value caml_memprof_start(value lv, value szv, value tracker_param)
{
  caml_failwith("Gc.Memprof.start: not implemented in multicore");
}

CAMLprim value caml_memprof_stop(value unit)
{
  caml_failwith("Gc.Memprof.stop: not implemented in multicore");
}

CAMLprim value caml_memprof_discard(value profile)
{
  caml_failwith("Gc.Memprof.discard: not implemented in multicore");
}
