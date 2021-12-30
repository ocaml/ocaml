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

/* Some runtime initialization functions that are common to bytecode
   and native code. */

#include <stdio.h>
#include "caml/backtrace.h"
#include "caml/memory.h"
#include "caml/callback.h"
#include "caml/major_gc.h"
#ifndef NATIVE_CODE
#include "caml/dynlink.h"
#endif
#include "caml/osdeps.h"
#include "caml/startup_aux.h"
#include "caml/prims.h"
#include "caml/version.h"

#ifdef _WIN32
extern void caml_win32_unregister_overflow_detection (void);
#endif

/* Configuration parameters and flags */

static struct caml_params params;
const struct caml_params* const caml_params = &params;

static void init_startup_params(void)
{
#ifndef NATIVE_CODE
  char_os * cds_file;
#endif

  params.init_percent_free = Percent_free_def;
  params.init_max_percent_free = Max_percent_free_def;
  params.init_minor_heap_wsz = Minor_heap_def;
  params.init_heap_chunk_sz = Heap_chunk_def;
  params.init_heap_wsz = Init_heap_def;
  params.init_custom_major_ratio = Custom_major_ratio_def;
  params.init_custom_minor_ratio = Custom_minor_ratio_def;
  params.init_custom_minor_max_bsz = Custom_minor_max_bsz_def;
  params.init_max_stack_wsz = Max_stack_def;
  params.init_fiber_wsz = (Stack_threshold * 2) / sizeof(value);
#ifdef DEBUG
  params.verb_gc = 0x3F;
#endif
#ifndef NATIVE_CODE
  cds_file = caml_secure_getenv(T("CAML_DEBUG_FILE"));
  if (cds_file != NULL) {
    params.cds_file = caml_stat_strdup_os(cds_file);
  }
#endif
  params.trace_level = 0;
  params.cleanup_on_exit = 0;
  params.print_magic = 0;
  params.print_config = 0;
}

static void scanmult (char_os *opt, uintnat *var)
{
  char_os mult = ' ';
  unsigned int val = 1;
  sscanf_os (opt, T("=%u%c"), &val, &mult);
  sscanf_os (opt, T("=0x%x%c"), &val, &mult);
  switch (mult) {
  case 'k':   *var = (uintnat) val * 1024; break;
  case 'M':   *var = (uintnat) val * (1024 * 1024); break;
  case 'G':   *var = (uintnat) val * (1024 * 1024 * 1024); break;
  default:    *var = (uintnat) val; break;
  }
}

void caml_parse_ocamlrunparam(void)
{
  char_os *opt = caml_secure_getenv (T("OCAMLRUNPARAM"));

  init_startup_params();

  if (opt == NULL) opt = caml_secure_getenv (T("CAMLRUNPARAM"));

  if (opt != NULL){
    while (*opt != '\0'){
      switch (*opt++){
      //case 'a': scanmult (opt, &p); caml_set_allocation_policy (p); break;
      case 'b': scanmult (opt, &params.backtrace_enabled); break;
      case 'c': scanmult (opt, &params.cleanup_on_exit); break;
      case 'e': scanmult (opt, &params.eventlog_enabled); break;
      case 'f': scanmult (opt, &params.init_fiber_wsz); break;
      case 'h': scanmult (opt, &params.init_heap_wsz); break;
      //case 'H': scanmult (opt, &caml_use_huge_pages); break;
      case 'i': scanmult (opt, &params.init_heap_chunk_sz); break;
      case 'l': scanmult (opt, &params.init_max_stack_wsz); break;
      case 'M': scanmult (opt, &params.init_custom_major_ratio); break;
      case 'm': scanmult (opt, &params.init_custom_minor_ratio); break;
      case 'n': scanmult (opt, &params.init_custom_minor_max_bsz); break;
      case 'o': scanmult (opt, &params.init_percent_free); break;
      case 'O': scanmult (opt, &params.init_max_percent_free); break;
      case 'p': scanmult (opt, &params.parser_trace); break;
      case 'R': break; /*  see stdlib/hashtbl.mli */
      case 's': scanmult (opt, &params.init_minor_heap_wsz); break;
      case 't': scanmult (opt, &params.trace_level); break;
      case 'v': scanmult (opt, &params.verb_gc); break;
      case 'V': scanmult (opt, &params.verify_heap); break;
      //case 'w': scanmult (opt, &caml_init_major_window); break;
      case 'W': scanmult (opt, &caml_runtime_warnings); break;
      case ',': continue;
      }
      --opt; /* to handle patterns like ",b=1" */
      while (*opt != '\0'){
        if (*opt++ == ',') break;
      }
    }
  }
}


/* The number of outstanding calls to caml_startup */
static int startup_count = 0;

/* Has the runtime been shut down already? */
static int shutdown_happened = 0;


int caml_startup_aux(int pooling)
{
  if (shutdown_happened == 1)
    caml_fatal_error("caml_startup was called after the runtime "
                     "was shut down with caml_shutdown");

  /* Second and subsequent calls are ignored,
     since the runtime has already started */
  startup_count++;
  if (startup_count > 1)
    return 0;

  if (pooling)
    caml_stat_create_pool();

  return 1;
}

static void call_registered_value(char* name)
{
  const value *f = caml_named_value(name);
  if (f != NULL)
    caml_callback_exn(*f, Val_unit);
}

CAMLexport void caml_shutdown(void)
{
  if (startup_count <= 0)
    caml_fatal_error("a call to caml_shutdown has no "
                     "corresponding call to caml_startup");

  /* Do nothing unless it's the last call remaining */
  startup_count--;
  if (startup_count > 0)
    return;

  call_registered_value("Pervasives.do_at_exit");
  call_registered_value("Thread.at_shutdown");
  caml_finalise_heap();
  caml_free_locale();
#ifndef NATIVE_CODE
  caml_free_shared_libs();
#endif
  caml_stat_destroy_pool();
#if defined(_WIN32) && defined(NATIVE_CODE)
  caml_win32_unregister_overflow_detection();
#endif

  shutdown_happened = 1;
}

void caml_init_exe_name(const char_os* exe_name)
{
  params.exe_name = exe_name;
}

void caml_init_section_table(const char* section_table,
                             asize_t section_table_size)
{
  params.section_table = section_table;
  params.section_table_size = section_table_size;
}

#ifndef NATIVE_CODE

static void do_print_help(void)
{
  printf("%s\n",
    "Usage: ocamlrun [<options>] [--] <executable> [<command-line>]\n"
    "Options are:\n"
    "  -b  Set runtime parameter b (detailed exception backtraces)\n"
    "  -config  Print configuration values and exit\n"
    "  -I <dir>  Add <dir> to the list of DLL search directories\n"
    "  -m  Print the magic number of <executable> and exit\n"
    "  -M  Print the magic number expected by this runtime and exit\n"
    "  -p  Print the names of the primitives known to this runtime\n"
    "  -t  Trace the execution of the bytecode interpreter (specify multiple\n"
    "      times to increase verbosity)\n"
    "  -v  Set runtime parameter v=61 (GC event information)\n"
    "  -version  Print version string and exit\n"
    "  -vnum  Print short version number and exit\n"
    "  -help  Display this list of options\n"
    "  --help  Display this list of options");
}

/* Print the specified error message followed by an end-of-line and exit */
extern void caml_command_error(char *msg, ...)
{
  va_list ap;
  va_start(ap, msg);
  vfprintf (stderr, msg, ap);
  va_end(ap);
  fprintf(stderr, "\n");
  exit(127);
}

/* Parse options on the command line */

extern int caml_parse_command_line(char_os **argv)
{
  int i, j, len, parsed;

  for(i = 1; argv[i] != NULL && argv[i][0] == '-'; i++) {
    len = strlen_os(argv[i]);
    parsed = 1;
    if (len == 2) {
      /* Single-letter options, e.g. -v */
      switch(argv[i][1]) {
      case '-':
        return i + 1;
        break;
      case 't':
        params.trace_level += 1; /* ignored unless DEBUG mode */
        break;
      case 'v':
        params.verb_gc = 0x001+0x004+0x008+0x010+0x020;
        break;
      case 'p':
        for (j = 0; caml_names_of_builtin_cprim[j] != NULL; j++)
          printf("%s\n", caml_names_of_builtin_cprim[j]);
        exit(0);
        break;
      case 'b':
        caml_record_backtraces(1);
        break;
      case 'I':
        if (argv[i + 1] != NULL) {
          caml_ext_table_add(&caml_shared_libs_path, argv[i + 1]);
          i++;
        } else {
          caml_command_error("option '-I' needs an argument.");
        }
        break;
      case 'm':
        params.print_magic = 1;
        break;
      case 'M':
        printf("%s\n", EXEC_MAGIC);
        exit(0);
        break;
      default:
        parsed = 0;
      }
    } else {
      /* Named options, e.g. -version */
      if (!strcmp_os(argv[i], T("-version"))) {
        printf("%s\n", "The OCaml runtime, version " OCAML_VERSION_STRING);
        printf ("The OCaml runtime, version " OCAML_VERSION_STRING "\n");
        exit(0);
      } else if (!strcmp_os(argv[i], T("-vnum"))) {
        printf("%s\n", OCAML_VERSION_STRING);
        exit(0);
      } else if (!strcmp_os(argv[i], T("-help")) ||
                 !strcmp_os(argv[i], T("--help"))) {
        do_print_help();
        exit(0);
      } else if (!strcmp_os(argv[i], T("-config"))) {
        params.print_config = 1;
      } else {
        parsed = 0;
      }
    }

    if (!parsed)
      caml_command_error("unknown option %s", caml_stat_strdup_of_os(argv[i]));
  }

  return i;
}

#endif /* not NATIVE_CODE */
