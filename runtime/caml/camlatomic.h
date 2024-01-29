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

/*
 * C11 atomics types and utility macros.
 */

#ifdef __cplusplus

extern "C++" {
#include <atomic>
typedef std::atomic<uintnat> atomic_uintnat;
typedef std::atomic<intnat> atomic_intnat;
using std::memory_order_relaxed;
using std::memory_order_acquire;
using std::memory_order_release;
using std::memory_order_acq_rel;
using std::memory_order_seq_cst;
}

#else

#include <stdatomic.h>
typedef _Atomic uintnat atomic_uintnat;
typedef _Atomic intnat atomic_intnat;

#endif

#ifdef CAML_INTERNALS

/* Loads and stores with acquire, release and relaxed semantics */

#define atomic_load_acquire(p)                    \
  atomic_load_explicit((p), memory_order_acquire)
#define atomic_load_relaxed(p)                    \
  atomic_load_explicit((p), memory_order_relaxed)
#define atomic_store_release(p, v)                      \
  atomic_store_explicit((p), (v), memory_order_release)
#define atomic_store_relaxed(p, v)                      \
  atomic_store_explicit((p), (v), memory_order_relaxed)

#endif /* CAML_INTERNALS */

#endif /* CAML_ATOMIC_H */
