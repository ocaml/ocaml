/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*         Xavier Leroy and Damien Doligez, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

/* Start-up code */

#include <stdio.h>
#include <stdlib.h>
#include "callback.h"
#include "custom.h"
#include "fail.h"
#include "gc.h"
#include "gc_ctrl.h"
#include "misc.h"
#include "mlvalues.h"
#include "osdeps.h"
#include "printexc.h"
#include "sys.h"
#ifdef HAS_UI
#include "ui.h"
#endif

extern int caml_parser_trace;
header_t caml_atom_table[256];
char * caml_static_data_start, * caml_static_data_end;
char * caml_code_area_start, * caml_code_area_end;

/* Initialize the atom table and the static data and code area limits. */

struct segment { char * begin; char * end; };

static void minmax_table(struct segment *table, char **min, char **max)
{
  int i;
  *min = table[0].begin;
  *max = table[0].end;
  for (i = 1; table[i].begin != 0; i++) {
    if (table[i].begin < *min) *min = table[i].begin;
    if (table[i].end > *max)   *max = table[i].end;
  }
}
  
static void init_atoms(void)
{
  int i;
  extern struct segment caml_data_segments[], caml_code_segments[];

  for (i = 0; i < 256; i++) caml_atom_table[i] = Make_header(0, i, Caml_white);
  minmax_table(caml_data_segments,
               &caml_static_data_start, &caml_static_data_end);
  minmax_table(caml_code_segments, &caml_code_area_start, &caml_code_area_end);
}

/* Configuration parameters and flags */

static unsigned long percent_free_init = Percent_free_def;
static unsigned long max_percent_free_init = Max_percent_free_def;
static unsigned long minor_heap_init = Minor_heap_def;
static unsigned long heap_chunk_init = Heap_chunk_def;
static unsigned long heap_size_init = Init_heap_def;
static unsigned long max_stack_init = Max_stack_def;

/* Parse the CAMLRUNPARAM variable */
/* The option letter for each runtime option is the first letter of the
   last word of the ML name of the option (see [stdlib/gc.mli]).
   Except for l (maximum stack size) and h (initial heap size).
*/
/* Note: option l is irrelevant to the native-code runtime. */

/* If you change these functions, see also their copy in byterun/startup.c */

static void scanmult (char *opt, long unsigned int *var)
{
  char mult = ' ';
  sscanf (opt, "=%lu%c", var, &mult);
  sscanf (opt, "=0x%lx%c", var, &mult);
  if (mult == 'k') *var = *var * 1024;
  if (mult == 'M') *var = *var * (1024 * 1024);
  if (mult == 'G') *var = *var * (1024 * 1024 * 1024);
}

static void parse_camlrunparam(void)
{
  char *opt = getenv ("OCAMLRUNPARAM");

  if (opt == NULL) opt = getenv ("CAMLRUNPARAM");

  if (opt != NULL){
    while (*opt != '\0'){
      switch (*opt++){
      case 's': scanmult (opt, &minor_heap_init); break;
      case 'i': scanmult (opt, &heap_chunk_init); break;
      case 'h': scanmult (opt, &heap_size_init); break;
      case 'l': scanmult (opt, &max_stack_init); break;
      case 'o': scanmult (opt, &percent_free_init); break;
      case 'O': scanmult (opt, &max_percent_free_init); break;
      case 'v': scanmult (opt, &caml_verb_gc); break;
      case 'p': caml_parser_trace = 1; break;
      }
    }
  }
}

/* These are termination hooks used by the systhreads library */
struct longjmp_buffer caml_termination_jmpbuf;
void (*caml_termination_hook)(void *) = NULL;

extern value caml_start_program (void);
extern void caml_init_ieee_floats (void);
extern void caml_init_signals (void);

void caml_main(char **argv)
{
  char * exe_name;
#ifdef __linux__
  static char proc_self_exe[256];
#endif
  value res;

  caml_init_ieee_floats();
  caml_init_custom_operations();
#ifdef DEBUG
  caml_verb_gc = 63;
#endif
  parse_camlrunparam();
  caml_init_gc (minor_heap_init, heap_size_init, heap_chunk_init,
                percent_free_init, max_percent_free_init);
  init_atoms();
  caml_init_signals();
  exe_name = argv[0];
  if (exe_name == NULL) exe_name = "";
#ifdef __linux__
  if (caml_executable_name(proc_self_exe, sizeof(proc_self_exe)) == 0)
    exe_name = proc_self_exe;
  else
    exe_name = caml_search_exe_in_path(exe_name);
#else
  exe_name = caml_search_exe_in_path(exe_name);
#endif
  caml_sys_init(exe_name, argv);
  if (sigsetjmp(caml_termination_jmpbuf.buf, 0)) {
    if (caml_termination_hook != NULL) caml_termination_hook(NULL);
    return;
  }
  res = caml_start_program();
  if (Is_exception_result(res))
    caml_fatal_uncaught_exception(Extract_exception(res));
}

void caml_startup(char **argv)
{
  caml_main(argv);
}
