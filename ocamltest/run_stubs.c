/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Sébastien Hinderer, projet Gallium, INRIA Paris           */
/*                                                                        */
/*   Copyright 2016 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

/* Stubs to let OCaml programs use the run library */

#define _GNU_SOURCE

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <sys/types.h>
#include <string.h>

#include "run.h"

#define CAML_NAME_SPACE

#include "caml/mlvalues.h"
#include "caml/memory.h"
#include "caml/io.h"

/* cstringvect: inspired by similar function in otherlibs/unix/cstringv.c */
char ** cstringvect(value arg)
{
  char ** res;
  mlsize_t size, i;

  size = Wosize_val(arg);
  /*
  for (i = 0; i < size; i++)
    if (! caml_string_is_c_safe(Field(arg, i)))
      unix_error(EINVAL, cmdname, Field(arg, i));
  */
  res = (char **) caml_stat_alloc((size + 1) * sizeof(char *));
  for (i = 0; i < size; i++) res[i] = String_val(Field(arg, i));
  res[size] = NULL;
  return res;
}

static void logToChannel(void *voidchannel, const char *fmt, ...)
{
  va_list ap;
  struct channel *channel = (struct channel *) voidchannel;
  char *text;
  int res;
  va_start(ap, fmt);
  res = vasprintf(&text, fmt, ap);
  va_end(ap);
  if (res <= 0) return;
  caml_putblock(channel, text, res);
  free(text);
}

CAMLprim value caml_run_command(value caml_settings)
{
  int res;
  array argv, envp;
  command_settings settings;

  CAMLparam1(caml_settings);
  settings.program = String_val(Field(caml_settings, 0));
  argv = cstringvect(Field(caml_settings, 1));
  settings.argv = &argv;
  /* envp = cstringvect(Field(caml_settings, 2)); */
  /* settings.envp = &envp; */
  settings.stdout_filename = String_val(Field(caml_settings, 2));
  settings.stderr_filename = String_val(Field(caml_settings, 3));
  settings.append = Bool_val(Field(caml_settings, 4));
  settings.timeout = Int_val(Field(caml_settings, 5));
  settings.logger = logToChannel;
  settings.loggerData = Channel(Field(caml_settings, 6));
  res = run_command(&settings);
  CAMLreturn(Val_int(res));
}
