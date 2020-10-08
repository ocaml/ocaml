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
#include "caml/codefrag.h"
#include "caml/debugger.h"
#include "caml/domain.h"
#include "caml/eventlog.h"
#include "caml/fail.h"
#include "caml/freelist.h"
#include "caml/gc.h"
#include "caml/gc_ctrl.h"
#include "caml/intext.h"
#include "caml/memory.h"
#include "caml/misc.h"
#include "caml/mlvalues.h"
#include "caml/osdeps.h"
#include "caml/printexc.h"
#include "caml/stack.h"
#include "caml/startup_aux.h"
#include "caml/sys.h"
#ifdef HAS_UI
#include "caml/ui.h"
#endif

extern int caml_parser_trace;
extern char caml_system__code_begin, caml_system__code_end;

/* Initialize the atom table and the static data and code area limits. */

struct segment { char * begin; char * end; };

static void init_static(void)
{
  extern struct segment caml_data_segments[], caml_code_segments[];

  char * caml_code_area_start, * caml_code_area_end;
  int i;

  caml_init_atom_table ();

  for (i = 0; caml_data_segments[i].begin != 0; i++) {
    /* PR#5509: we must include the zero word at end of data segment,
       because pointers equal to caml_data_segments[i].end are static data. */
    if (caml_page_table_add(In_static_data,
                            caml_data_segments[i].begin,
                            caml_data_segments[i].end + sizeof(value)) != 0)
      caml_fatal_error("not enough memory for initial page table");
  }

  caml_code_area_start = caml_code_segments[0].begin;
  caml_code_area_end = caml_code_segments[0].end;
  for (i = 1; caml_code_segments[i].begin != 0; i++) {
    if (caml_code_segments[i].begin < caml_code_area_start)
      caml_code_area_start = caml_code_segments[i].begin;
    if (caml_code_segments[i].end > caml_code_area_end)
      caml_code_area_end = caml_code_segments[i].end;
  }
  /* Register the code in the table of code fragments */
  caml_register_code_fragment(caml_code_area_start,
                              caml_code_area_end,
                              DIGEST_LATER, NULL);
  /* Also register the glue code written in assembly */
  caml_register_code_fragment(&caml_system__code_begin,
                              &caml_system__code_end,
                              DIGEST_IGNORE, NULL);
}

/* These are termination hooks used by the systhreads library */
struct longjmp_buffer caml_termination_jmpbuf;
void (*caml_termination_hook)(void *) = NULL;

extern value caml_start_program (caml_domain_state*);
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

  /* Initialize the domain */
  caml_init_domain();
  /* Determine options */
#ifdef DEBUG
  caml_verb_gc = 0x3F;
#endif
  caml_parse_ocamlrunparam();
  CAML_EVENTLOG_INIT();
#ifdef DEBUG
  caml_gc_message (-1, "### OCaml runtime: debug mode ###\n");
#endif
  if (caml_cleanup_on_exit)
    pooling = 1;
  if (!caml_startup_aux(pooling))
    return Val_unit;

  caml_init_frame_descriptors();
  caml_init_locale();
#if defined(_MSC_VER) && __STDC_SECURE_LIB__ >= 200411L
  caml_install_invalid_parameter_handler();
#endif
  caml_init_custom_operations();
  Caml_state->top_of_stack = &tos;
  caml_init_gc (caml_init_minor_heap_wsz, caml_init_heap_wsz,
                caml_init_heap_chunk_sz, caml_init_percent_free,
                caml_init_max_percent_free, caml_init_major_window,
                caml_init_custom_major_ratio, caml_init_custom_minor_ratio,
                caml_init_custom_minor_max_bsz);
  init_static();
  caml_init_signals();
#ifdef _WIN32
  caml_win32_overflow_detection();
#endif
  caml_init_backtrace();
  caml_debugger_init (); /* force debugger.o stub to be linked */
  exe_name = argv[0];
  if (exe_name == NULL) exe_name = T("");
  proc_self_exe = caml_executable_name();
  if (proc_self_exe != NULL)
    exe_name = proc_self_exe;
  else
    exe_name = caml_search_exe_in_path(exe_name);
  caml_sys_init(exe_name, argv);
  if (sigsetjmp(caml_termination_jmpbuf.buf, 0)) {
    if (caml_termination_hook != NULL) caml_termination_hook(NULL);
    return Val_unit;
  }
  return caml_start_program(Caml_state);
}

value caml_startup_exn(char_os **argv)
{
  return caml_startup_common(argv, /* pooling */ 0);
}

void caml_startup(char_os **argv)
{
  value res = caml_startup_exn(argv);
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
