/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  Automatique.  Distributed only by permission.                      */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

/* Callbacks from C to Caml */

#include <string.h>
#include "callback.h"
#include "memory.h"
#include "mlvalues.h"

#ifndef NATIVE_CODE

/* Bytecode callbacks */

#include "interp.h"
#include "instruct.h"
#include "fix_code.h"
#include "stacks.h"

int callback_depth = 0;

static opcode_t callback_code[] = { ACC, 0, APPLY, 0, POP, 1, STOP };

#ifdef THREADED_CODE

static int callback_code_threaded = 0;

static void thread_callback(void)
{
  thread_code(callback_code, sizeof(callback_code));
  callback_code_threaded = 1;
}

#define Init_callback() if (!callback_code_threaded) thread_callback()

#else

#define Init_callback()

#endif

value callbackN(value closure, int narg, value args[])
{
  value res;
  int i;

  Assert(narg + 4 <= 256);
  Init_callback();
  extern_sp -= narg + 4;
  for (i = 0; i < narg; i++) extern_sp[i] = args[i]; /* arguments */
  extern_sp[narg] = (value) (callback_code + 4); /* return address */
  extern_sp[narg + 1] = Val_unit;    /* environment */
  extern_sp[narg + 2] = Val_long(0); /* extra args */
  extern_sp[narg + 3] = closure;
  callback_code[1] = narg + 3;
  callback_code[3] = narg;
  res = interprete(callback_code, sizeof(callback_code));
  return res;
}

value callback(value closure, value arg1)
{
  value arg[1];
  arg[0] = arg1;
  return callbackN(closure, 1, arg);
}

value callback2(value closure, value arg1, value arg2)
{
  value arg[2];
  arg[0] = arg1;
  arg[1] = arg2;
  return callbackN(closure, 2, arg);
}

value callback3(value closure, value arg1, value arg2, value arg3)
{
  value arg[3];
  arg[0] = arg1;
  arg[1] = arg2;
  arg[2] = arg3;
  return callbackN(closure, 3, arg);
}

#else

/* Native-code callbacks.  callback[123] are implemented in asm. */

value callbackN(value closure, int narg, value args[])
{
  value res;
  int i;

  res = closure;
  Begin_roots1(res)
    Begin_roots_block(args, narg)
      for (i = 0; i < narg; /*nothing*/) {
        /* Pass as many arguments as possible */
        switch (narg - i) {
        case 1:
          res = callback(res, args[i]);
          i += 1;
          break;
        case 2:
          res = callback2(res, args[i], args[i + 1]);
          i += 2;
          break;
        default:
          res = callback3(res, args[i], args[i + 1], args[i + 2]);
          i += 3;
          break;
        }
      }
    End_roots();
  End_roots();
  return res;
}

#endif

/* Naming of Caml values */

struct named_value {
  value val;
  struct named_value * next;
  char name[1];
};

#define Named_value_size 13

static struct named_value * named_value_table[Named_value_size] = { NULL, };

static unsigned int hash_value_name(char *name)
{
  unsigned int h;
  for (h = 0; *name != 0; name++) h = h * 19 + *name;
  return h % Named_value_size;
}

value register_named_value(value vname, value val) /* ML */
{
  struct named_value * nv;
  char * name = String_val(vname);
  unsigned int h = hash_value_name(name);

  nv = (struct named_value *)
         stat_alloc(sizeof(struct named_value) + strlen(name));
  strcpy(nv->name, name);
  nv->val = val;
  nv->next = named_value_table[h];
  named_value_table[h] = nv;
  register_global_root(&nv->val);
  return Val_unit;
}

value * caml_named_value(char *name)
{
  struct named_value * nv;
  for (nv = named_value_table[hash_value_name(name)];
       nv != NULL;
       nv = nv->next) {
    if (strcmp(name, nv->name) == 0) return &nv->val;
  }
  return NULL;
}
