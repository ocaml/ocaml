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


