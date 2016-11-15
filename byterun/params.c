#include <stdio.h>
#include <string.h>
#include "caml/misc.h"
#include "caml/params.h"
#include "caml/version.h"
#include "caml/prims.h"
#include "caml/dynlink.h"

/* Configuration parameters and flags */

struct caml_startup_params caml_startup_params;

static void init_startup_params()
{
  struct caml_startup_params *p = &caml_startup_params;
  p->percent_free_init = Percent_free_def;
  p->max_percent_free_init = Max_percent_free_def;
  p->minor_heap_init = Minor_heap_def;
  p->heap_chunk_init = Heap_chunk_def;
  p->heap_size_init = Init_heap_def;
  p->max_stack_init = Max_stack_def;
#ifdef PROFILING
  p->fiber_wsz_init = Profile_slop + (Stack_threshold * 2) / sizeof(value);
#else
  p->fiber_wsz_init = (Stack_threshold * 2) / sizeof(value);
#endif
#ifdef DEBUG
  p->verb_gc = 1;
#endif
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

  opt = getenv ("OCAMLRUNPARAM");

  if (opt == NULL) opt = getenv ("CAMLRUNPARAM");

  if (opt != NULL){
    while (*opt != '\0'){
      switch (*opt++){
      case 'b': caml_startup_params.backtrace_enabled_init = 1; break;
      case 'h': scanmult (opt, &caml_startup_params.heap_size_init); break;
      case 'i': scanmult (opt, &caml_startup_params.heap_chunk_init); break;
      case 'l': scanmult (opt, &caml_startup_params.max_stack_init); break;
      case 'o': scanmult (opt, &caml_startup_params.percent_free_init); break;
      case 'O': scanmult (opt, &caml_startup_params.max_percent_free_init); break;
      case 'p': caml_startup_params.parser_trace = 1; break;
      /* case 'R': see stdlib/hashtbl.mli */
      case 's': scanmult (opt, &caml_startup_params.minor_heap_init); break;
#ifdef DEBUG
      case 't': caml_startup_params.trace_flag = 1; break;
#endif
      case 'v': scanmult (opt, &caml_startup_params.verb_gc); break;
      case 'f': scanmult (opt, &caml_startup_params.fiber_wsz_init); break;
      case 'e': caml_startup_params.eventlog_enabled = 1; break;
      }
    }
  }
}


/* Parse options on the command line */

int caml_parse_command_line(char **argv)
{
  int i, j;

  for(i = 1; argv[i] != NULL && argv[i][0] == '-'; i++) {
    switch(argv[i][1]) {
#ifdef DEBUG
    case 't':
      caml_startup_params.trace_flag++;
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
        caml_startup_params.verb_gc = 0x001+0x004+0x008+0x010+0x020;
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
      caml_startup_params.backtrace_enabled_init = 1;
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
