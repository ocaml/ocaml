/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*      KC Sivaramakrishnan, Indian Institute of Technology, Madras       */
/*                Stephen Dolan, University of Cambridge                  */
/*                                                                        */
/*   Copyright 2018 Indian Institute of Technology, Madras                */
/*   Copyright 2018 University of Cambridge                               */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/
#ifndef CAML_ATOMIC_H
#define CAML_ATOMIC_H

#include "config.h"

/* On platforms supporting C11 atomics, this file just includes <stdatomic.h>.

   On other platforms, this file includes platform-specific stubs for
   the subset of C11 atomics needed by the OCaml runtime
 */

#ifdef __cplusplus

extern "C++" {
#include <atomic>
#define ATOMIC_UINTNAT_INIT(x) (x)
typedef std::atomic<uintnat> atomic_uintnat;
typedef std::atomic<intnat> atomic_intnat;
using std::memory_order_relaxed;
using std::memory_order_acquire;
using std::memory_order_release;
using std::memory_order_acq_rel;
using std::memory_order_seq_cst;
}

#elif defined(HAS_STDATOMIC_H)

#include <stdatomic.h>
#define ATOMIC_UINTNAT_INIT(x) (x)
typedef _Atomic uintnat atomic_uintnat;
typedef _Atomic intnat atomic_intnat;

#elif defined(__GNUC__)

/* Support for versions of gcc which have built-in atomics but do not
   expose stdatomic.h (e.g. gcc 4.8) */
typedef enum memory_order {
  memory_order_relaxed = __ATOMIC_RELAXED,
  memory_order_acquire = __ATOMIC_ACQUIRE,
  memory_order_release = __ATOMIC_RELEASE,
  memory_order_acq_rel = __ATOMIC_ACQ_REL,
  memory_order_seq_cst = __ATOMIC_SEQ_CST
} memory_order;

#define ATOMIC_UINTNAT_INIT(x) { (x) }
typedef struct { uintnat repr; } atomic_uintnat;
typedef struct { intnat repr; } atomic_intnat;

#define atomic_load_explicit(x, m) __atomic_load_n(&(x)->repr, (m))
#define atomic_load(x) atomic_load_explicit((x), memory_order_seq_cst)
#define atomic_store_explicit(x, v, m) __atomic_store_n(&(x)->repr, (v), (m))
#define atomic_store(x, v) atomic_store_explicit((x), (v), memory_order_seq_cst)
#define atomic_compare_exchange_strong(x, oldv, newv) \
  __atomic_compare_exchange_n( \
    &(x)->repr, \
    (oldv), (newv), 0, \
    memory_order_seq_cst, memory_order_seq_cst)
#define atomic_exchange(x, newv) \
  __atomic_exchange_n(&(x)->repr, (newv), memory_order_seq_cst)
#define atomic_fetch_add(x, n) \
  __atomic_fetch_add(&(x)->repr, (n), memory_order_seq_cst)
#define atomic_fetch_or(x, n) \
  __atomic_fetch_or(&(x)->repr, (n), memory_order_seq_cst)
#define atomic_thread_fence __atomic_thread_fence

#else
#error "C11 atomics are unavailable on this platform. See camlatomic.h"
#endif

#endif /* CAML_ATOMIC_H */
