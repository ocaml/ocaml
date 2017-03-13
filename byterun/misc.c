/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*         Xavier Leroy and Damien Doligez, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <execinfo.h>
#include <stdlib.h>

#include "caml/config.h"
#include "caml/misc.h"
#include "caml/memory.h"
#include "caml/domain.h"
#include "caml/startup.h"

#if defined(DEBUG) || defined(NATIVE_CODE)

void print_trace (void)
{
  void *array[10];
  size_t size;
  char **strings;
  size_t i;

  size = backtrace (array, 10);
  strings = backtrace_symbols (array, size);

  caml_gc_log ("Obtained %zd stack frames.", size);

  for (i = 0; i < size; i++)
    caml_gc_log ("%s", strings[i]);

  free (strings);
}

int caml_failed_assert (char * expr, char * file, int line)
{
  struct domain* self = caml_domain_self ();
  fprintf (stderr, "[%02d] file %s; line %d ### Assertion failed: %s\n",
           self ? self->id : -1, file, line, expr);
  print_trace ();
  fflush (stderr);
  abort();
  return 1; /* not reached */
}

#endif /* DEBUG */

void caml_gc_log (char *msg, ...)
{
  va_list args;
  va_start (args, msg);

  char fmtbuf[512];

  if (caml_params->verb_gc) {
    struct domain* self = caml_domain_self();
    sprintf(fmtbuf, "[%02d] %s\n", self ? self->id : -1, msg);
    vfprintf(stderr, fmtbuf, args);
  }
  fflush(stderr);

  va_end (args);
}

CAMLexport void caml_fatal_error (const char *msg)
{
  fprintf (stderr, "%s", msg);
  exit(2);
}

CAMLexport void caml_fatal_error_arg (const char *fmt, const char *arg)
{
  fprintf (stderr, fmt, arg);
  exit(2);
}

CAMLexport void caml_fatal_error_arg2 (const char *fmt1, const char *arg1,
                                       const char *fmt2, const char *arg2)
{
  fprintf (stderr, fmt1, arg1);
  fprintf (stderr, fmt2, arg2);
  exit(2);
}



void caml_ext_table_init(struct ext_table * tbl, int init_capa)
{
  tbl->size = 0;
  tbl->capacity = init_capa;
  tbl->contents = caml_stat_alloc(sizeof(void *) * init_capa);
}

int caml_ext_table_add(struct ext_table * tbl, void * data)
{
  int res;
  if (tbl->size >= tbl->capacity) {
    tbl->capacity *= 2;
    tbl->contents =
      caml_stat_resize(tbl->contents, sizeof(void *) * tbl->capacity);
  }
  res = tbl->size;
  tbl->contents[res] = data;
  tbl->size++;
  return res;
}

void caml_ext_table_free(struct ext_table * tbl, int free_entries)
{
  int i;
  if (free_entries)
    for (i = 0; i < tbl->size; i++) caml_stat_free(tbl->contents[i]);
  caml_stat_free(tbl->contents);
}

CAMLexport char * caml_strdup(const char * s)
{
  size_t slen = strlen(s);
  char * res = caml_stat_alloc(slen + 1);
  memcpy(res, s, slen + 1);
  return res;
}

CAMLexport char * caml_strconcat(int n, ...)
{
  va_list args;
  char * res, * p;
  size_t len;
  int i;

  len = 0;
  va_start(args, n);
  for (i = 0; i < n; i++) {
    const char * s = va_arg(args, const char *);
    len += strlen(s);
  }
  va_end(args);
  res = caml_stat_alloc(len + 1);
  va_start(args, n);
  p = res;
  for (i = 0; i < n; i++) {
    const char * s = va_arg(args, const char *);
    size_t l = strlen(s);
    memcpy(p, s, l);
    p += l;
  }
  va_end(args);
  *p = 0;
  return res;
}
