/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*          Xavier Leroy and Damien Doligez, INRIA Rocquencourt           */
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

/* Start-up code */

#include <stdio.h>
#include <stdlib.h>
#include "caml/callback.h"
#include "caml/backtrace.h"
#include "caml/custom.h"
#include "caml/debugger.h"
#include "caml/fail.h"
#include "caml/gc.h"
#include "caml/gc_ctrl.h"
#include "caml/intext.h"
#include "caml/memory.h"
#include "caml/misc.h"
#include "caml/mlvalues.h"
#include "caml/osdeps.h"
#include "caml/printexc.h"
#include "caml/stack.h"
#include "caml/sys.h"
#include "caml/startup_aux.h"
#include "caml/fiber.h"
#ifdef WITH_SPACETIME
#include "caml/spacetime.h"
#endif
#ifdef HAS_UI
#include "caml/ui.h"
#endif

extern int caml_parser_trace;
char * caml_code_area_start, * caml_code_area_end;

/* Initialize the static data and code area limits. */

struct segment { char * begin; char * end; };

static void init_segments(void)
{
  extern struct segment caml_data_segments[], caml_code_segments[];
  int i;
  struct code_fragment * cf;

  caml_code_area_start = caml_code_segments[0].begin;
  caml_code_area_end = caml_code_segments[0].end;
  for (i = 1; caml_code_segments[i].begin != 0; i++) {
    if (caml_code_segments[i].begin < caml_code_area_start)
      caml_code_area_start = caml_code_segments[i].begin;
    if (caml_code_segments[i].end > caml_code_area_end)
      caml_code_area_end = caml_code_segments[i].end;
  }
  /* Register the code in the table of code fragments */
  cf = caml_stat_alloc(sizeof(struct code_fragment));
  cf->code_start = caml_code_area_start;
  cf->code_end = caml_code_area_end;
  cf->digest_computed = 0;
  caml_ext_table_init(&caml_code_fragments_table, 8);
  caml_ext_table_add(&caml_code_fragments_table, cf);
}

/* These are termination hooks used by the systhreads library */
struct longjmp_buffer caml_termination_jmpbuf;
void (*caml_termination_hook)(void *) = NULL;

extern value caml_start_program (char*);
extern void caml_init_ieee_floats (void);
extern void caml_init_signals (void);
#ifdef _WIN32
extern void caml_win32_overflow_detection (void);
#endif

#if defined(_MSC_VER) && __STDC_SECURE_LIB__ >= 200411L

/* PR 4887: avoid crash box of windows runtime on some system calls */
extern void caml_install_invalid_parameter_handler();

#endif

value caml_startup_common(char_os **argv, int pooling)
{
  char_os * exe_name, * proc_self_exe;
  char tos;

  CAML_INIT_DOMAIN_STATE;

  /* Determine options */
  caml_parse_ocamlrunparam();
#ifdef DEBUG
  caml_gc_message (-1, "### OCaml runtime: debug mode ###\n");
#endif
  if (caml_params->cleanup_on_exit)
    pooling = 1;
  if (!caml_startup_aux(pooling))
    return Val_unit;

#ifdef WITH_SPACETIME
  caml_spacetime_initialize();
#endif
  caml_init_ieee_floats();
#if defined(_MSC_VER) && __STDC_SECURE_LIB__ >= 200411L
  caml_install_invalid_parameter_handler();
#endif
  caml_init_custom_operations();
  caml_init_gc ();

  if (caml_params->backtrace_enabled_init)
    caml_record_backtrace(Val_int(1));

  /* Capture 16-byte aligned (ceil) system_stack_high */
  Caml_state->system_stack_high =
    (char*)((((uintnat)&tos + 16) >> 4) << 4);

  init_segments();
  caml_init_signals();
#ifdef _WIN32
  caml_win32_overflow_detection();
#endif
  caml_debugger_init (); /* force debugger.o stub to be linked */
  exe_name = argv[0];
  if (exe_name == NULL) exe_name = _T("");
  proc_self_exe = caml_executable_name();
  if (proc_self_exe != NULL)
    exe_name = proc_self_exe;
  else
    exe_name = caml_search_exe_in_path(exe_name);
  caml_init_argv(exe_name, argv);
  if (sigsetjmp(caml_termination_jmpbuf.buf, 0)) {
    if (caml_termination_hook != NULL) caml_termination_hook(NULL);
    return Val_unit;
  }
  caml_init_main_stack();
  caml_start_program(Caml_state->young_ptr);
}

value caml_startup_exn(char_os **argv)
{
  return caml_startup_common(argv, /* pooling */ 0);
}

void caml_startup(char_os **argv)
{
  value res = caml_startup_exn(argv);
  caml_maybe_print_stats(Val_unit);
  if (Is_exception_result(res))
    caml_fatal_uncaught_exception(Extract_exception(res));
}

void caml_main(char_os **argv)
{
  caml_startup(argv);
}

value caml_startup_pooled_exn(char_os **argv)
{
  return caml_startup_common(argv, /* pooling */ 1);
}

void caml_startup_pooled(char_os **argv)
{
  value res = caml_startup_pooled_exn(argv);
  if (Is_exception_result(res))
    caml_fatal_uncaught_exception(Extract_exception(res));
}
