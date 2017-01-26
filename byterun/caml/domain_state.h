#ifndef CAML_DOMAIN_STATE_H
#define CAML_DOMAIN_STATE_H

#include <stddef.h>
#ifdef __APPLE__
#include <pthread.h>
#endif

#include "misc.h"

typedef struct caml_root_private* caml_root;

/* This structure sits in the TLS area and is also accessed efficiently
 * via native code, which is why the indeces are important */
struct caml_domain_state {
#define DOMAIN_STATE(idx, type, name) type name;
#include "domain_state.tbl"
#ifndef NATIVE_CODE
  /* Bytecode TLS vars, not used for native code */
  #define BYTE_DOMAIN_STATE(type, name) type name;
  #include "byte_domain_state.tbl"
  #undef BYTE_DOMAIN_STATE
#endif
#undef DOMAIN_STATE
};

#ifdef __APPLE__
  CAMLextern pthread_key_t caml_domain_state_key;
  #define CAML_INIT_DOMAIN_STATE (pthread_key_create(&caml_domain_state_key, NULL))
  #define CAML_DOMAIN_STATE \
      ((struct caml_domain_state*) pthread_getspecific(caml_domain_state_key))
  #define SET_CAML_DOMAIN_STATE(x) \
      (pthread_setspecific(caml_domain_state_key, x))
#else
  CAMLextern __thread struct caml_domain_state* __caml_domain_state;
  #define CAML_INIT_DOMAIN_STATE
  #define CAML_DOMAIN_STATE __caml_domain_state
  #define SET_CAML_DOMAIN_STATE(x) (__caml_domain_state = (x))
#endif


#endif
