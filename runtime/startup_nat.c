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
#include <string.h>
#include <errno.h>
#include <fcntl.h>
#include "caml/config.h"
#ifdef HAS_UNISTD
#include <unistd.h>
#endif
#include "caml/callback.h"
#include "caml/backtrace.h"
#include "caml/custom.h"
#include "caml/debugger.h"
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
#ifdef WITH_SPACETIME
#include "caml/spacetime.h"
#endif
#ifdef HAS_UI
#include "caml/ui.h"
#endif

#ifndef O_BINARY
#define O_BINARY 0
#endif

extern int caml_parser_trace;
CAMLexport header_t caml_atom_table[256];
char * caml_code_area_start, * caml_code_area_end;
struct ext_table caml_code_fragments_table;

/* Initialize the atom table and the static data and code area limits. */

struct segment { char * begin; char * end; };

static void init_static(void)
{
  extern struct segment caml_data_segments[], caml_code_segments[];
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
}

/* Initialize the table of code fragments and add the code of the executable
   as first fragment. */

static void init_code_fragments(char_os * exe_name)
{
  struct code_fragment * cf;
  int fd;

  cf = caml_stat_alloc(sizeof(struct code_fragment));
  cf->code_start = caml_code_area_start;
  cf->code_end = caml_code_area_end;
  /* Issue #5942: it is not enough to hash the code, global data
     must also be included in the digest.  However, global data,
     once loaded into main memory, varies from run to run of a
     given executable, owing to ASLR.  Hence, we use a digest of
     the whole executable file as digest for the main code fragment.
     This digest is computed on-demand from a file descriptor
     opened on the executable file. */
  /* Discussion #8615: sometimes the main executable file is not in OCaml
     and dynamically loads the OCaml compiled code from a shared library.  
     In this case, hashing the main executable file does not give us
     enough guarantees.  So, we also hash the bytes in the code
     section, like in the previous hashing schema.
     Issue #5942 still remains in this unlikely case. */
  fd = caml_open_file(exe_name, 0);
  if (fd != -1) {
    cf->digest_status = FROM_FILE_AND_CODE_AREA;
    cf->filedescr = fd;
  } else {
    cf->digest_status = DIGEST_ERROR;
    if (caml_runtime_warnings_active()) {
      char * name = caml_stat_strdup_of_os(exe_name);
      fprintf(stderr, "[ocaml] cannot open executable file %s: %s\n",
              name, strerror(errno));
      caml_stat_free(name);
    }
  }
  caml_ext_table_init(&caml_code_fragments_table, 8);
  caml_ext_table_add(&caml_code_fragments_table, cf);
}

/* These are termination hooks used by the systhreads library */
struct longjmp_buffer caml_termination_jmpbuf;
void (*caml_termination_hook)(void *) = NULL;

extern value caml_start_program (void);
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

  /* Determine options */
#ifdef DEBUG
  caml_verb_gc = 0x3F;
#endif
  caml_parse_ocamlrunparam();
#ifdef DEBUG
  caml_gc_message (-1, "### OCaml runtime: debug mode ###\n");
#endif
  if (caml_cleanup_on_exit)
    pooling = 1;
  if (!caml_startup_aux(pooling))
    return Val_unit;

#ifdef WITH_SPACETIME
  caml_spacetime_initialize();
#endif
  caml_init_frame_descriptors();
  caml_init_ieee_floats();
  caml_init_locale();
#if defined(_MSC_VER) && __STDC_SECURE_LIB__ >= 200411L
  caml_install_invalid_parameter_handler();
#endif
  caml_init_custom_operations();
  caml_top_of_stack = &tos;
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
  init_code_fragments(exe_name);
  caml_sys_init(exe_name, argv);
  if (sigsetjmp(caml_termination_jmpbuf.buf, 0)) {
    if (caml_termination_hook != NULL) caml_termination_hook(NULL);
    return Val_unit;
  }
  return caml_start_program();
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
