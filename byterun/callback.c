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

/* Bytecode callbacks (implemented in asm for the native code compiler) */

#ifndef NATIVE_CODE

#include "interp.h"
#include "instruct.h"
#include "fix_code.h"
#include "stacks.h"

int callback_depth = 0;

static opcode_t callback1_code[] = { ACC1, APPLY1, POP, 1, STOP };
static opcode_t callback2_code[] = { ACC2, APPLY2, POP, 1, STOP };
static opcode_t callback3_code[] = { ACC3, APPLY3, POP, 1, STOP };

#ifdef THREADED_CODE

static int callback_code_threaded = 0;

static void thread_callback(void)
{
  thread_code(callback1_code, sizeof(callback1_code));
  thread_code(callback2_code, sizeof(callback2_code));
  thread_code(callback3_code, sizeof(callback3_code));
  callback_code_threaded = 1;
}

#define Init_callback() if (!callback_code_threaded) thread_callback()

#else

#define Init_callback()

#endif

value callback(value closure, value arg)
{
  value res;
  Init_callback();
  extern_sp -= 2;
  extern_sp[0] = arg;
  extern_sp[1] = closure;
  res = interprete(callback1_code, sizeof(callback1_code));
  return res;
}

value callback2(value closure, value arg1, value arg2)
{
  value res;
  Init_callback();
  extern_sp -= 3;
  extern_sp[0] = arg1;
  extern_sp[1] = arg2;
  extern_sp[2] = closure;
  res = interprete(callback2_code, sizeof(callback2_code));
  return res;
}

value callback3(value closure, value arg1, value arg2, value arg3)
{
  value res;
  Init_callback();
  extern_sp -= 4;
  extern_sp[0] = arg1;
  extern_sp[1] = arg2;
  extern_sp[2] = arg3;
  extern_sp[3] = closure;
  res = interprete(callback3_code, sizeof(callback3_code));
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
