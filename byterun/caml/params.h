#ifndef CAML_PARAMS_H
#define CAML_PARAMS_H

#include "mlvalues.h"
#include "exec.h"

/* readonly after startup */
struct caml_params {
  const char* exe_name;
  const char* const* main_argv;

  /* for meta.c */
  const char* section_table;
  asize_t section_table_size;

  const char* cds_file;

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
};

extern const struct caml_params* const caml_params;

/* Called only from startup.c */
void caml_init_startup_params();
int caml_parse_command_line(char** argv);
void caml_init_argv(const char* exe_name, char** main_argv);
void caml_init_section_table(const char* section_table,
                             asize_t section_table_size);

#endif /* CAML_PARAMS_H */
