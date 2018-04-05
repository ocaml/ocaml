#ifndef Caml_state_H
#define Caml_state_H

#include <stddef.h>
#ifdef __APPLE__
#include <pthread.h>
#endif

#include "misc.h"

typedef struct caml_root_private* caml_root;

/* This structure sits in the TLS area and is also accessed efficiently
 * via native code, which is why the indices are important */
typedef struct {
#define DOMAIN_STATE(type, name) CAMLalign(8) type name;
#include "domain_state.tbl"
#ifndef NATIVE_CODE
  /* Bytecode TLS vars, not used for native code */
  #define BYTE_DOMAIN_STATE(type, name) type name;
  #include "byte_domain_state.tbl"
  #undef BYTE_DOMAIN_STATE
#endif
#undef DOMAIN_STATE
} caml_domain_state;

enum {
  Domain_state_num_fields =
#define DOMAIN_STATE(type, name) + 1
#include "domain_state.tbl"
#undef DOMAIN_STATE

#ifndef NATIVE_CODE
  ,
  Byte_domain_state_num_fields =
#define BYTE_DOMAIN_STATE(type, name) + 1
#include "byte_domain_state.tbl"
#undef BYTE_DOMAIN_STATE
#endif
};

/* Check that the structure was laid out without padding,
   since the runtime assumes this in computing offsets */
CAML_STATIC_ASSERT(
  sizeof(caml_domain_state) ==
   (Domain_state_num_fields
#ifndef NATIVE_CODE
     + Byte_domain_state_num_fields
#endif
   ) * 8);

#ifdef __APPLE__
  CAMLextern pthread_key_t caml_domain_state_key;
  CAMLextern void caml_init_domain_state_key(void);
  #define CAML_INIT_DOMAIN_STATE caml_init_domain_state_key()
  #define Caml_state \
      ((caml_domain_state*) pthread_getspecific(caml_domain_state_key))
  #define SET_Caml_state(x) \
      (pthread_setspecific(caml_domain_state_key, x))
#else
  CAMLextern __thread caml_domain_state* caml_domain_curr_state;
  #define CAML_INIT_DOMAIN_STATE
  #define Caml_state caml_domain_curr_state
  #define SET_Caml_state(x) (caml_domain_curr_state = (x))
#endif

#endif
