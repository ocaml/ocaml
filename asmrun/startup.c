/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*         Xavier Leroy and Damien Doligez, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  Automatique.  Distributed only by permission.                      */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

/* Start-up code */

#include <stdio.h>
#include <stdlib.h>
#include "fail.h"
#include "gc.h"
#include "gc_ctrl.h"
#include "misc.h"
#include "mlvalues.h"
#include "sys.h"
#ifdef HAS_UI
#include "ui.h"
#endif

header_t atom_table[256];
char * static_data_start, * static_data_end;
char * code_area_start, * code_area_end;

/* Initialize the atom table and the static data and code area limits. */

struct segment { char * begin; char * end; };

static void minmax_table(table, min, max)
     struct segment table[];
     char ** min, ** max;
{
  int i;
  *min = table[0].begin;
  *max = table[0].end;
  for (i = 1; table[i].begin != 0; i++) {
    if (table[i].begin < *min) *min = table[i].begin;
    if (table[i].end > *max)   *max = table[i].end;
  }
}
  
static void init_atoms()
{
  int i;
  extern struct segment caml_data_segments[], caml_code_segments[];

  for (i = 0; i < 256; i++) atom_table[i] = Make_header(0, i, White);
  minmax_table(caml_data_segments, &static_data_start, &static_data_end);
  minmax_table(caml_code_segments, &code_area_start, &code_area_end);
}

/* Configuration parameters and flags */

static unsigned long verbose_init = 0;
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

static void scanmult (opt, var)
     char *opt;
     unsigned long *var;
{
  char mult = ' ';
  sscanf (opt, "=%lu%c", var, &mult);
  if (mult == 'k') *var = *var * 1024;
  if (mult == 'M') *var = *var * (1024 * 1024);
  if (mult == 'G') *var = *var * (1024 * 1024 * 1024);
}

static void parse_camlrunparam()
{
  char *opt = getenv ("CAMLRUNPARAM");
  if (opt != NULL){
    while (*opt != '\0'){
      switch (*opt++){
      case 's': scanmult (opt, &minor_heap_init); break;
      case 'i': scanmult (opt, &heap_chunk_init); break;
      case 'h': scanmult (opt, &heap_size_init); break;
      case 'l': scanmult (opt, &max_stack_init); break;
      case 'o': scanmult (opt, &percent_free_init); break;
      case 'O': scanmult (opt, &max_percent_free_init); break;
      case 'v': scanmult (opt, &verbose_init); break;
      }
    }
  }
}

extern void caml_start_program P((void));
extern void init_ieee_floats P((void));
extern void init_signals P((void));

void caml_main(argv)
     char ** argv;
{
  init_ieee_floats();
#ifdef DEBUG
  verbose_init = 1;
#endif
  parse_camlrunparam();
  init_gc (minor_heap_init, heap_size_init, heap_chunk_init,
           percent_free_init, max_percent_free_init, verbose_init);
  init_atoms();
  init_signals();
  sys_init(argv);
  caml_start_program();
}

void caml_startup(argv)
     char ** argv;
{
  caml_main(argv);
}
