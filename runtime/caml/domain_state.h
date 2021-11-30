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

#include <stddef.h>
#include <stdio.h>
#include "misc.h"
#include "mlvalues.h"

#define NUM_EXTRA_PARAMS 64
typedef value extra_params_area[NUM_EXTRA_PARAMS];

/* This structure sits in the TLS area and is also accessed efficiently
 * via native code, which is why the indices are important */

typedef struct {
#ifdef CAML_NAME_SPACE
#define DOMAIN_STATE(type, name) CAMLalign(8) type name;
#else
#define DOMAIN_STATE(type, name) CAMLalign(8) type _##name;
#endif
#include "domain_state.tbl"
#undef DOMAIN_STATE
} caml_domain_state;

enum {
  Domain_state_num_fields =
#define DOMAIN_STATE(type, name) + 1
#include "domain_state.tbl"
#undef DOMAIN_STATE
};

#ifdef CAML_NAME_SPACE
#define LAST_DOMAIN_STATE_MEMBER extra_params
#else
#define LAST_DOMAIN_STATE_MEMBER _extra_params
#endif

/* Check that the structure was laid out without padding,
   since the runtime assumes this in computing offsets */
CAML_STATIC_ASSERT(
    offsetof(caml_domain_state, LAST_DOMAIN_STATE_MEMBER) ==
    (Domain_state_num_fields - 1) * 8);

CAMLextern caml_domain_state* Caml_state;
#ifdef CAML_NAME_SPACE
#define Caml_state_field(field) Caml_state->field
#else
#define Caml_state_field(field) Caml_state->_##field
#endif

#endif /* CAML_STATE_H */
