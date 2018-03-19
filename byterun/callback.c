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
#include "caml/callback.h"
#include "caml/fail.h"
#include "caml/memory.h"
#include "caml/mlvalues.h"
#include "caml/platform.h"

static __thread int callback_depth = 0;

#ifndef NATIVE_CODE

/* Bytecode callbacks */

#include "caml/interp.h"
#include "caml/instruct.h"
#include "caml/fix_code.h"
#include "caml/fiber.h"

static opcode_t callback_code[] = { ACC, 0, APPLY, 0, POP, 1, STOP };

static void init_callback_code(void)
{
#ifdef THREADED_CODE
  caml_thread_code(callback_code, sizeof(callback_code));
#endif
}

CAMLexport value caml_callbackN_exn(value closure, int narg, value args[])
{
  CAMLparam0();
  CAMLlocal1(parent_stack);
  int i;
  value res;
  caml_domain_state* domain_state = Caml_state;
  parent_stack = Stack_parent(domain_state->current_stack);
  Stack_parent(domain_state->current_stack) = Val_unit;

  Assert(narg + 4 <= 256);
  domain_state->extern_sp -= narg + 4;
  for (i = 0; i < narg; i++) domain_state->extern_sp[i] = args[i]; /* arguments */

  opcode_t code[7] = {
    callback_code[0], narg + 3,
    callback_code[2], narg,
    callback_code[4], callback_code[5], callback_code[6]
  };

  domain_state->extern_sp[narg] = Val_pc (code + 4); /* return address */
  domain_state->extern_sp[narg + 1] = Val_unit;    /* environment */
  domain_state->extern_sp[narg + 2] = Val_long(0); /* extra args */
  domain_state->extern_sp[narg + 3] = closure;
  res = caml_interprete(code, sizeof(code));
  if (Is_exception_result(res)) domain_state->extern_sp += narg + 4; /* PR#1228 */

  Assert(Stack_parent(domain_state->current_stack) == Val_unit);
  Stack_parent(domain_state->current_stack) = parent_stack;
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

#else /* Nativecode callbacks */

static void init_callback_code(void)
{
}

value caml_callback_asm(char* young_ptr, value closure, value arg);
value caml_callback2_asm(char* young_ptr, value closure, value arg1, value arg2);
value caml_callback3_asm(char* young_ptr, value closure, value* args);

CAMLexport value caml_callback_exn(value closure, value arg)
{
  return caml_callback_asm(Caml_state->young_ptr, closure, arg);
}

CAMLexport value caml_callback2_exn(value closure, value arg1, value arg2)
{
  return caml_callback2_asm(Caml_state->young_ptr, closure, arg1, arg2);
}

CAMLexport value caml_callback3_exn(value closure,
                                    value arg1, value arg2, value arg3)
{
  /* can only pass 4 args in registers on Windows, so we use an array */
  value args[] = {arg1, arg2, arg3};
  return caml_callback3_asm(Caml_state->young_ptr, closure, args);
}

/* Native-code callbacks.  caml_callback[123]_exn are implemented in asm. */

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
  value res = caml_callback_exn(closure, arg);
  if (Is_exception_result(res)) caml_raise(Extract_exception(res));
  return res;
}

CAMLexport value caml_callback2 (value closure, value arg1, value arg2)
{
  value res = caml_callback2_exn(closure, arg1, arg2);
  if (Is_exception_result(res)) caml_raise(Extract_exception(res));
  return res;
}

CAMLexport value caml_callback3 (value closure, value arg1, value arg2,
                                 value arg3)
{
  value res = caml_callback3_exn(closure, arg1, arg2, arg3);
  if (Is_exception_result(res)) caml_raise(Extract_exception(res));
  return res;
}

CAMLexport value caml_callbackN (value closure, int narg, value args[])
{
  value res = caml_callbackN_exn(closure, narg, args);
  if (Is_exception_result(res)) caml_raise(Extract_exception(res));
  return res;
}

/* Naming of OCaml values */

struct named_value {
  caml_root val;
  struct named_value * next;
  char name[1];
};

#define Named_value_size 13

static struct named_value * named_value_table[Named_value_size] = { NULL, };
static caml_plat_mutex named_value_lock = CAML_PLAT_MUTEX_INITIALIZER;

void caml_init_callbacks() {
  init_callback_code();
}

static unsigned int hash_value_name(char const *name)
{
  unsigned int h;
  for (h = 0; *name != 0; name++) h = h * 19 + *name;
  return h % Named_value_size;
}

CAMLprim value caml_register_named_value(value vname, value val)
{
  struct named_value * nv;
  char * name = String_val(vname);
  size_t namelen = strlen(name);
  unsigned int h = hash_value_name(name);
  int found = 0;

  caml_plat_lock(&named_value_lock);
  for (nv = named_value_table[h]; nv != NULL; nv = nv->next) {
    if (strcmp(name, nv->name) == 0) {
      caml_modify_root(nv->val, val);
      found = 1;
      break;
    }
  }
  if (!found) {
    nv = (struct named_value *)
      caml_stat_alloc(sizeof(struct named_value) + namelen);
    memcpy(nv->name, name, namelen + 1);
    nv->val = caml_create_root(val);
    nv->next = named_value_table[h];
    named_value_table[h] = nv;
  }
  caml_plat_unlock(&named_value_lock);
  return Val_unit;
}

CAMLexport caml_root caml_named_root(char const *name)
{
  struct named_value * nv;
  caml_root ret = NULL;
  caml_plat_lock(&named_value_lock);
  for (nv = named_value_table[hash_value_name(name)];
       nv != NULL;
       nv = nv->next) {
    if (strcmp(name, nv->name) == 0){
      ret = nv->val;
      break;
    }
  }
  caml_plat_unlock(&named_value_lock);

  return ret;
}

CAMLexport int caml_get_callback_depth ()
{
  return callback_depth;
}

void caml_incr_callback_depth ()
{
  callback_depth++;
}

void caml_decr_callback_depth ()
{
  callback_depth--;
}

CAMLexport void caml_iterate_named_values(caml_named_action f)
{
  int i;
  for(i = 0; i < Named_value_size; i++){
    struct named_value * nv;
    for (nv = named_value_table[i]; nv != NULL; nv = nv->next) {
      f( nv->val, nv->name );
    }
  }
}
