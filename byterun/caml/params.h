#ifndef CAML_PARAMS_H
#define CAML_PARAMS_H

#include "mlvalues.h"
#include "exec.h"

/* readonly after startup */
extern struct caml_startup_params {
  char* exe_name;
  char** main_argv;

  uintnat verb_gc;
  int parser_trace;
  int trace_flag;
  int eventlog_enabled;

  uintnat percent_free_init;
  uintnat max_percent_free_init;
  uintnat minor_heap_init;
  uintnat heap_chunk_init;
  uintnat heap_size_init;
  uintnat max_stack_init;
  uintnat fiber_wsz_init;
  int backtrace_enabled_init;
} caml_startup_params;

void caml_init_startup_params();
int caml_parse_command_line(char** argv);

#endif /* CAML_PARAMS_H */
