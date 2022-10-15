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
#include "caml/custom.h"
#include "caml/codefrag.h"
#include "caml/debugger.h"
#include "caml/runtime_events.h"
#include "caml/fiber.h"
#include "caml/fail.h"
#include "caml/gc.h"
#include "caml/gc_ctrl.h"
#include "caml/intext.h"
#include "caml/memory.h"
#include "caml/misc.h"
#include "caml/mlvalues.h"
#include "caml/osdeps.h"
#include "caml/printexc.h"
#include "caml/signals.h"
#include "caml/stack.h"
#include "caml/startup_aux.h"
#include "caml/sys.h"

extern int caml_parser_trace;
extern char caml_system__code_begin, caml_system__code_end;
/* The two symbols above are defined in runtime/$ARCH.S.
   They use the old `__` separator convention because the new convention
   gives `caml_system.code_begin`, which is not a valid C identifier. */

/* Initialize the static data and code area limits. */

struct segment { char * begin; char * end; };

static void init_segments(void)
{
  extern struct segment caml_code_segments[];
  char * caml_code_area_start, * caml_code_area_end;
  int i;

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

extern value caml_start_program (caml_domain_state*);
#ifdef _WIN32
extern void caml_win32_overflow_detection (void);
#endif

#ifdef _MSC_VER

/* PR 4887: avoid crash box of windows runtime on some system calls */
extern void caml_install_invalid_parameter_handler(void);

#endif

value caml_startup_common(char_os **argv, int pooling)
{
  char_os * exe_name, * proc_self_exe;
  value res;

  /* Determine options */
  caml_parse_ocamlrunparam();

#ifdef DEBUG
  caml_gc_message (-1, "### OCaml runtime: debug mode ###\n");
#endif
  if (caml_params->cleanup_on_exit)
    pooling = 1;
  if (!caml_startup_aux(pooling))
    return Val_unit;

  caml_init_codefrag();
  caml_init_locale();
#ifdef _MSC_VER
  caml_install_invalid_parameter_handler();
#endif
  caml_init_custom_operations();
  caml_init_os_params();
  caml_init_gc ();

  /* runtime_events's init can cause a stop-the-world pause, so it must be done
     after we've initialised the garbage collector */
  CAML_RUNTIME_EVENTS_INIT();

  init_segments();
  caml_init_signals();
#ifdef _WIN32
  caml_win32_overflow_detection();
#endif
  caml_debugger_init (); /* force debugger.o stub to be linked */
  exe_name = argv[0];
  if (exe_name == NULL) exe_name = T("");
  proc_self_exe = caml_executable_name();
  if (proc_self_exe != NULL)
    exe_name = proc_self_exe;
  else
    exe_name = caml_search_exe_in_path(exe_name);
  caml_sys_init(exe_name, argv);
  caml_maybe_expand_stack();
  res = caml_start_program(Caml_state);
  caml_terminate_signals();
  return res;
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
