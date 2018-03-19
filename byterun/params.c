#define CAML_INTERNALS

#include <stdio.h>
#include <string.h>
#include "caml/misc.h"
#include "caml/params.h"
#include "caml/version.h"
#include "caml/osdeps.h"
#include "caml/prims.h"
#include "caml/dynlink.h"
#include "caml/domain.h"

/* Configuration parameters and flags */

static struct caml_params params;
const struct caml_params* const caml_params = &params;

static void init_startup_params()
{
  params.percent_free_init = Percent_free_def;
  params.max_percent_free_init = Max_percent_free_def;
  params.minor_heap_init = Minor_heap_def;
  params.heap_chunk_init = Heap_chunk_def;
  params.heap_size_init = Init_heap_def;
  params.max_stack_init = Max_stack_def;
#ifdef PROFILING
  params.fiber_wsz_init = Profile_slop + (Stack_threshold * 2) / sizeof(value);
#else
  params.fiber_wsz_init = (Stack_threshold * 2) / sizeof(value);
#endif
#ifdef DEBUG
  params.verb_gc = 1;
#endif
#ifndef NATIVE_CODE
  params.cds_file = getenv("CAML_DEBUG_FILE");
  if (params.cds_file != NULL) {
    params.cds_file = caml_strdup(params.cds_file);
  }
#endif
  params.profile_slop_wsz = 0;
}

CAMLprim value caml_maybe_print_stats (value v)
{                             Assert (v == Val_unit);
  if (params.print_stats)
    caml_print_stats ();
  return Val_unit;
}

/* Parse the OCAMLRUNPARAM variable */
/* The option letter for each runtime option is the first letter of the
   last word of the ML name of the option (see [stdlib/gc.mli]).
   Except for l (maximum stack size) and h (initial heap size).
*/

static void scanmult (char *opt, uintnat *var)
{
  char mult = ' ';
  unsigned int val;
  sscanf (opt, "=%u%c", &val, &mult);
  sscanf (opt, "=0x%x%c", &val, &mult);
  switch (mult) {
  case 'k':   *var = (uintnat) val * 1024; break;
  case 'M':   *var = (uintnat) val * 1024 * 1024; break;
  case 'G':   *var = (uintnat) val * 1024 * 1024 * 1024; break;
  default:    *var = (uintnat) val; break;
  }
}

void caml_init_startup_params(void)
{
  char *opt;

  init_startup_params();

  opt = caml_secure_getenv ("OCAMLRUNPARAM");

  if (opt == NULL) opt = caml_secure_getenv ("CAMLRUNPARAM");

  if (opt != NULL){
    while (*opt != '\0'){
      switch (*opt++){
      case 'b': params.backtrace_enabled_init = 1; break;
      case 'h': scanmult (opt, &params.heap_size_init); break;
      case 'i': scanmult (opt, &params.heap_chunk_init); break;
      case 'l': scanmult (opt, &params.max_stack_init); break;
      case 'o': scanmult (opt, &params.percent_free_init); break;
      case 'O': scanmult (opt, &params.max_percent_free_init); break;
      case 'p': params.parser_trace = 1; break;
      case 'R': break; /* case 'R': see stdlib/hashtbl.mli */
      case 's': scanmult (opt, &params.minor_heap_init); break;
      case 't': params.trace_flag = 1; break;
      case 'v': scanmult (opt, &params.verb_gc); break;
      case 'V': params.verify_heap = 1; break;
      case 'f': scanmult (opt, &params.fiber_wsz_init); break;
      case 'w': scanmult (opt, &params.profile_slop_wsz); break;
      case 'e': params.eventlog_enabled = 1; break;
      case 'S': params.print_stats = 1; break;
      }
    }
  }
}


/* Parse options on the command line */

int caml_parse_command_line(char **argv)
{
  int i;
#ifndef NATIVE_CODE
  int j;
#endif

  for(i = 1; argv[i] != NULL && argv[i][0] == '-'; i++) {
    switch(argv[i][1]) {
#ifdef DEBUG
    case 't':
      params.trace_flag++;
      break;
#endif
    case 'v':
      if (!strcmp (argv[i], "-version")){
        printf ("The OCaml runtime, version " OCAML_VERSION_STRING "\n");
        exit (0);
      }else if (!strcmp (argv[i], "-vnum")){
        printf (OCAML_VERSION_STRING "\n");
        exit (0);
      }else{
        params.verb_gc = 0x001+0x004+0x008+0x010+0x020;
      }
      break;
#ifndef NATIVE_CODE
    case 'p':
      for (j = 0; caml_names_of_builtin_cprim[j] != NULL; j++)
        printf("%s\n", caml_names_of_builtin_cprim[j]);
      exit(0);
      break;
#endif
    case 'b':
      params.backtrace_enabled_init = 1;
      break;
#ifndef NATIVE_CODE
    case 'I':
      if (argv[i + 1] != NULL) {
        caml_ext_table_add(&caml_shared_libs_path, argv[i + 1]);
        i++;
      }
      break;
#endif
    default:
      caml_fatal_error_arg("Unknown option %s.\n", argv[i]);
    }
  }
  return i;
}

void caml_init_argv(const char* exe_name, char** main_argv)
{
  params.exe_name = exe_name;
  params.main_argv = (const char* const*)main_argv;
}

void caml_init_section_table(const char* section_table,
                             asize_t section_table_size)
{
  params.section_table = section_table;
  params.section_table_size = section_table_size;
}
