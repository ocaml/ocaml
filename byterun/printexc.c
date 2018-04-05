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

/* Print an uncaught exception and abort */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "caml/backtrace.h"
#include "caml/callback.h"
#include "caml/debugger.h"
#include "caml/fail.h"
#include "caml/misc.h"
#include "caml/mlvalues.h"
#include "caml/printexc.h"
#include "caml/memory.h"

struct stringbuf {
  char * ptr;
  char * end;
  char data[256];
};

static void add_char(struct stringbuf *buf, char c)
{
  if (buf->ptr < buf->end) *(buf->ptr++) = c;
}

static void add_string(struct stringbuf *buf, const char *s)
{
  int len = strlen(s);
  if (buf->ptr + len > buf->end) len = buf->end - buf->ptr;
  if (len > 0) memmove(buf->ptr, s, len);
  buf->ptr += len;
}

CAMLexport char * caml_format_exception(value exn)
{
  mlsize_t start, i;
  struct stringbuf buf;
  char intbuf[64];
  char * res;
  CAMLparam1(exn);
  CAMLlocal4(bucket, v, exnclass, field1);

  buf.ptr = buf.data;
  buf.end = buf.data + sizeof(buf.data) - 1;
  /* An exception class is a value with tag Object_tag, whose first
     field is a string naming the exception.
     Exceptions that take parameters (e.g. Invalid_argument) are blocks
     with tag 0, where the first field is the exception class.
     Exceptions without parameters (e.g. Not_found) are just the exception
     class. */
  if (Tag_val(exn) == 0) {
    /* Field 0 of exn is the exception class, which is immutable */
    exnclass = Field_imm(exn, 0);
    add_string(&buf, String_val(Field_imm(exnclass, 0)));
    /* Check for exceptions in the style of Match_failure and Assert_failure */
    if (Wosize_val(exn) == 2) {
      caml_read_field(exn, 1, &field1);
    } else {
      field1 = Val_unit;
    }
    if (Is_block(field1) &&
        Tag_val(field1) == 0 &&
        caml_is_special_exception(exnclass)) {
      bucket = field1;
      start = 0;
    } else {
      bucket = exn;
      start = 1;
    }
    add_char(&buf, '(');
    for (i = start; i < Wosize_val(bucket); i++) {
      if (i > start) add_string(&buf, ", ");
      caml_read_field(bucket, i, &v);
      if (Is_long(v)) {
        snprintf(intbuf, sizeof(intbuf),
                 "%" ARCH_INTNAT_PRINTF_FORMAT "d", Long_val(v));
        add_string(&buf, intbuf);
      } else if (Tag_val(v) == String_tag) {
        add_char(&buf, '"');
        add_string(&buf, String_val(v));
        add_char(&buf, '"');
      } else {
        add_char(&buf, '_');
      }
    }
    add_char(&buf, ')');
  } else {
    /* Exception without parameters */
    exnclass = exn;
    add_string(&buf, String_val(Field_imm(exnclass, 0)));
  }

  *buf.ptr = 0;              /* Terminate string */
  i = buf.ptr - buf.data + 1;
  res = caml_stat_alloc_noexc(i);
  if (res == NULL) CAMLreturnT (char*, NULL);
  memmove(res, buf.data, i);
  CAMLreturnT (char*, res);
}


#ifdef NATIVE_CODE
#  define DEBUGGER_IN_USE 0
#else
#  define DEBUGGER_IN_USE caml_debugger_in_use
#endif

/* Default C implementation in case the OCaml one is not registered. */
static void default_fatal_uncaught_exception(value exn)
{
  char * msg;
  caml_root at_exit;
  int saved_backtrace_active;
  intnat saved_backtrace_pos;
  caml_domain_state* domain_state = Caml_state;

  /* Build a string representation of the exception */
  msg = caml_format_exception(exn);
  /* Perform "at_exit" processing, ignoring all exceptions that may
     be triggered by this */
  saved_backtrace_active = domain_state->backtrace_active;
  saved_backtrace_pos = domain_state->backtrace_pos;
  domain_state->backtrace_active = 0;
  at_exit = caml_named_root("Pervasives.do_at_exit");
  if (at_exit) caml_callback_exn(caml_read_root(at_exit), Val_unit);
  domain_state->backtrace_active = saved_backtrace_active;
  domain_state->backtrace_pos = saved_backtrace_pos;
  /* Display the uncaught exception */
  fprintf(stderr, "Fatal error: exception %s\n", msg);
  caml_stat_free(msg);
  /* Display the backtrace if available */
  if (domain_state->backtrace_active && !DEBUGGER_IN_USE)
    caml_print_exception_backtrace();
}

int caml_abort_on_uncaught_exn = 0; /* see afl.c */

void caml_fatal_uncaught_exception(value exn)
{
  caml_root handle_uncaught_exception =
    caml_named_root("Printexc.handle_uncaught_exception");
  if (handle_uncaught_exception)
    /* [Printexc.handle_uncaught_exception] does not raise exception. */
    caml_callback2(caml_read_root(handle_uncaught_exception), exn, Val_bool(DEBUGGER_IN_USE));
  else
    default_fatal_uncaught_exception(exn);
  /* Terminate the process */
  if (caml_abort_on_uncaught_exn) {
    abort();
  } else {
    CAML_SYS_EXIT(2);
    exit(2); /* Second exit needed for the Noreturn flag */
  }
}
