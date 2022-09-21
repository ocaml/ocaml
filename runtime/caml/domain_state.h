/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*      KC Sivaramakrishnan, Indian Institute of Technology, Madras       */
/*                Stephen Dolan, University of Cambridge                  */
/*                                                                        */
/*   Copyright 2019 Indian Institute of Technology, Madras                */
/*   Copyright 2019 University of Cambridge                               */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#ifndef CAML_STATE_H
#define CAML_STATE_H

#ifdef __APPLE__
#include <pthread.h>
#endif
#include <stddef.h>
#include <stdio.h>

#include "misc.h"

#define NUM_EXTRA_PARAMS 64
typedef value extra_params_area[NUM_EXTRA_PARAMS];

/* This structure sits in the TLS area and is also accessed efficiently
 * via native code, which is why the indices are important */
typedef struct {
#define DOMAIN_STATE(type, name) CAMLalign(8) type name;
#include "domain_state.tbl"
#undef DOMAIN_STATE
} caml_domain_state;

enum {
  Domain_state_num_fields =
#define DOMAIN_STATE(type, name) + 1
#include "domain_state.tbl"
#undef DOMAIN_STATE
};

#define LAST_DOMAIN_STATE_MEMBER extra_params

/* Check that the structure was laid out without padding,
   since the runtime assumes this in computing offsets */
CAML_STATIC_ASSERT(
    offsetof(caml_domain_state, LAST_DOMAIN_STATE_MEMBER) ==
    (Domain_state_num_fields - 1) * 8);

#ifdef __APPLE__
  CAMLextern pthread_key_t caml_domain_state_key;
  CAMLextern void caml_init_domain_state_key(void);
  #define CAML_INIT_DOMAIN_STATE caml_init_domain_state_key()
  #define Caml_state_opt \
      ((caml_domain_state*) pthread_getspecific(caml_domain_state_key))
  #define SET_Caml_state(x) \
      (pthread_setspecific(caml_domain_state_key, x))
#else
  CAMLextern __thread caml_domain_state* caml_state;
  #define Caml_state_opt caml_state
  #define CAML_INIT_DOMAIN_STATE
  #define SET_Caml_state(x) (caml_state = (x))
#endif

#define Caml_state (CAMLassert(Caml_state_opt != NULL), Caml_state_opt)

CAMLnoreturn_start
CAMLextern void caml_bad_caml_state(void)
CAMLnoreturn_end;

/* This check is performed regardless of debug mode. It is placed once
   at every code path starting from entry points of the public C API,
   whenever the load of Caml_state_opt can be eliminated by CSE (or if
   the function is not performance-sensitive). */
#define Caml_check_caml_state()                                         \
  (CAMLlikely(Caml_state_opt != NULL) ? (void)0 :                       \
   caml_bad_caml_state())

#define Caml_state_field(field) (Caml_state->field)

#endif /* CAML_STATE_H */
