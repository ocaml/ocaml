/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 1996 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#define CAML_INTERNALS

/* Callbacks from C to OCaml */

#include <string.h>
#include "caml/alloc.h"
#include "caml/callback.h"
#include "caml/codefrag.h"
#include "caml/fail.h"
#include "caml/fiber.h"
#include "caml/memory.h"
#include "caml/mlvalues.h"
#include "caml/platform.h"

/*
 * These functions are to ensure effects are handled correctly inside
 * callbacks. There are two aspects:
 *  - we clear the stack parent for a callback to force an Effect.Unhandled
 *  exception rather than effects being passed over the callback
 *  - we register the stack parent as a local root while the callback
 * is executing to ensure that the garbage collector follows the
 * stack parent
 */
Caml_inline value save_and_clear_stack_parent(caml_domain_state* domain_state) {
  struct stack_info* parent_stack = Stack_parent(domain_state->current_stack);
  value cont = caml_alloc_1(Cont_tag, Val_ptr(parent_stack));
  Stack_parent(domain_state->current_stack) = NULL;
  return cont;
}

Caml_inline void restore_stack_parent(caml_domain_state* domain_state,
                                      value cont) {
  struct stack_info* parent_stack = Ptr_val(Op_val(cont)[0]);
  CAMLassert(Stack_parent(domain_state->current_stack) == NULL);
  Stack_parent(domain_state->current_stack) = parent_stack;
}


#ifndef NATIVE_CODE

/* Bytecode callbacks */

#include "caml/interp.h"
#include "caml/instruct.h"
#include "caml/fix_code.h"
#include "caml/fiber.h"

static __thread opcode_t callback_code[] = { ACC, 0, APPLY, 0, POP, 1, STOP };

static __thread int callback_code_inited = 0;

static void init_callback_code(void)
{
  caml_register_code_fragment((char *) callback_code,
                              (char *) callback_code + sizeof(callback_code),
                              DIGEST_IGNORE, NULL);
#ifdef THREADED_CODE
  caml_thread_code(callback_code, sizeof(callback_code));
#endif
  callback_code_inited = 1;
}

CAMLexport value caml_callbackN_exn(value closure, int narg, value args[])
{
  CAMLparam1(closure);
  CAMLxparamN(args, narg);
  CAMLlocal1(cont);
  value res;
  int i;
  caml_domain_state* domain_state = Caml_state;

  CAMLassert(narg + 4 <= 256);
  domain_state->current_stack->sp -= narg + 4;
  for (i = 0; i < narg; i++)
    domain_state->current_stack->sp[i] = args[i]; /* arguments */

  if (!callback_code_inited) init_callback_code();

  callback_code[1] = narg + 3;
  callback_code[3] = narg;

  domain_state->current_stack->sp[narg] =
                     (value)(callback_code + 4); /* return address */
  domain_state->current_stack->sp[narg + 1] = Val_unit;    /* environment */
  domain_state->current_stack->sp[narg + 2] = Val_long(0); /* extra args */
  domain_state->current_stack->sp[narg + 3] = closure;

  cont = save_and_clear_stack_parent(domain_state);

  res = caml_interprete(callback_code, sizeof(callback_code));
  if (Is_exception_result(res))
    domain_state->current_stack->sp += narg + 4; /* PR#3419 */

  restore_stack_parent(domain_state, cont);

  CAMLreturn (res);
}

CAMLexport value caml_callback_exn(value closure, value arg1)
{
  value arg[1];
  arg[0] = arg1;
  return caml_callbackN_exn(closure, 1, arg);
}

CAMLexport value caml_callback2_exn(value closure, value arg1, value arg2)
{
  value arg[2];
  arg[0] = arg1;
  arg[1] = arg2;
  return caml_callbackN_exn(closure, 2, arg);
}

CAMLexport value caml_callback3_exn(value closure,
                               value arg1, value arg2, value arg3)
{
  value arg[3];
  arg[0] = arg1;
  arg[1] = arg2;
  arg[2] = arg3;
  return caml_callbackN_exn(closure, 3, arg);
}

#else

/* Native-code callbacks.  caml_callback[123]_asm are implemented in asm. */

static void init_callback_code(void)
{
}

typedef value (callback_stub)(caml_domain_state* state,
                              value closure,
                              value* args);

callback_stub caml_callback_asm, caml_callback2_asm, caml_callback3_asm;

CAMLexport value caml_callback_exn(value closure, value arg)
{
  Caml_check_caml_state();
  caml_domain_state* domain_state = Caml_state;
  caml_maybe_expand_stack();

  if (Stack_parent(domain_state->current_stack)) {
    CAMLparam2 (closure, arg);
    CAMLlocal1 (cont);
    value res;

    cont = save_and_clear_stack_parent(domain_state);
    res = caml_callback_asm(domain_state, closure, &arg);
    restore_stack_parent(domain_state, cont);

    CAMLreturn (res);
  } else {
    return caml_callback_asm(domain_state, closure, &arg);
  }
}

CAMLexport value caml_callback2_exn(value closure, value arg1, value arg2)
{
  Caml_check_caml_state();
  value args[] = {arg1, arg2};
  caml_domain_state* domain_state = Caml_state;
  caml_maybe_expand_stack();

  if (Stack_parent(domain_state->current_stack)) {
    CAMLparam3 (closure, arg1, arg2);
    CAMLlocal1 (cont);
    value res;

    cont = save_and_clear_stack_parent(domain_state);
    res = caml_callback2_asm(domain_state, closure, args);
    restore_stack_parent(domain_state, cont);

    CAMLreturn (res);
  } else {
    return caml_callback2_asm(domain_state, closure, args);
  }
}

CAMLexport value caml_callback3_exn(value closure,
                                    value arg1, value arg2, value arg3)
{
  Caml_check_caml_state();
  value args[] = {arg1, arg2, arg3};
  caml_domain_state* domain_state = Caml_state;
  caml_maybe_expand_stack();

  if (Stack_parent(domain_state->current_stack))  {
    CAMLparam4 (closure, arg1, arg2, arg3);
    CAMLlocal1 (cont);
    value res;

    cont = save_and_clear_stack_parent(domain_state);
    res = caml_callback3_asm(domain_state, closure, args);
    restore_stack_parent(domain_state, cont);

    CAMLreturn (res);
  } else {
    return caml_callback3_asm(domain_state, closure, args);
  }
}

CAMLexport value caml_callbackN_exn(value closure, int narg, value args[])
{
  CAMLparam1 (closure);
  CAMLxparamN (args, narg);
  CAMLlocal1 (res);
  int i;

  res = closure;
  for (i = 0; i < narg; /*nothing*/) {
    /* Pass as many arguments as possible */
    switch (narg - i) {
    case 1:
      res = caml_callback_exn(res, args[i]);
      if (Is_exception_result(res)) CAMLreturn (res);
      i += 1;
      break;
    case 2:
      res = caml_callback2_exn(res, args[i], args[i + 1]);
      if (Is_exception_result(res)) CAMLreturn (res);
      i += 2;
      break;
    default:
      res = caml_callback3_exn(res, args[i], args[i + 1], args[i + 2]);
      if (Is_exception_result(res)) CAMLreturn (res);
      i += 3;
      break;
    }
  }
  CAMLreturn (res);
}

#endif

/* Exception-propagating variants of the above */

CAMLexport value caml_callback (value closure, value arg)
{
  return caml_raise_if_exception(caml_callback_exn(closure, arg));
}

CAMLexport value caml_callback2 (value closure, value arg1, value arg2)
{
  return caml_raise_if_exception(caml_callback2_exn(closure, arg1, arg2));
}

CAMLexport value caml_callback3 (value closure, value arg1, value arg2,
                                 value arg3)
{
  return caml_raise_if_exception(caml_callback3_exn(closure, arg1, arg2, arg3));
}

CAMLexport value caml_callbackN (value closure, int narg, value args[])
{
  return caml_raise_if_exception(caml_callbackN_exn(closure, narg, args));
}

/* Naming of OCaml values */

struct named_value {
  value val;
  struct named_value * next;
  char name[1];
};

#define Named_value_size 13

static struct named_value * named_value_table[Named_value_size] = { NULL, };
static caml_plat_mutex named_value_lock = CAML_PLAT_MUTEX_INITIALIZER;

void caml_init_callbacks(void)
{
  init_callback_code();
}

static unsigned int hash_value_name(char const *name)
{
  unsigned int h;
  /* "djb2" hash function */
  for (h = 5381; *name != 0; name++) h = h * 33 + *name;
  return h % Named_value_size;
}

CAMLprim value caml_register_named_value(value vname, value val)
{
  struct named_value * nv;
  const char * name = String_val(vname);
  size_t namelen = strlen(name);
  unsigned int h = hash_value_name(name);
  int found = 0;

  caml_plat_lock(&named_value_lock);
  for (nv = named_value_table[h]; nv != NULL; nv = nv->next) {
    if (strcmp(name, nv->name) == 0) {
      caml_modify_generational_global_root(&nv->val, val);
      found = 1;
      break;
    }
  }
  if (!found) {
    nv = (struct named_value *)
      caml_stat_alloc(sizeof(struct named_value) + namelen);
    memcpy(nv->name, name, namelen + 1);
    nv->val = val;
    nv->next = named_value_table[h];
    named_value_table[h] = nv;
    caml_register_generational_global_root(&nv->val);
  }
  caml_plat_unlock(&named_value_lock);
  return Val_unit;
}

CAMLexport const value* caml_named_value(char const *name)
{
  struct named_value * nv;
  caml_plat_lock(&named_value_lock);
  for (nv = named_value_table[hash_value_name(name)];
       nv != NULL;
       nv = nv->next) {
    if (strcmp(name, nv->name) == 0){
      caml_plat_unlock(&named_value_lock);
      return &nv->val;
    }
  }
  caml_plat_unlock(&named_value_lock);
  return NULL;
}

CAMLexport void caml_iterate_named_values(caml_named_action f)
{
  int i;
  caml_plat_lock(&named_value_lock);
  for(i = 0; i < Named_value_size; i++){
    struct named_value * nv;
    for (nv = named_value_table[i]; nv != NULL; nv = nv->next) {
      f( Op_val(nv->val), nv->name );
    }
  }
  caml_plat_unlock(&named_value_lock);
}
