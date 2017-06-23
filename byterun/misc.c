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

#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include "caml/config.h"
#include "caml/misc.h"
#include "caml/memory.h"
#include "caml/osdeps.h"
#include "caml/version.h"

caml_timing_hook caml_major_slice_begin_hook = NULL;
caml_timing_hook caml_major_slice_end_hook = NULL;
caml_timing_hook caml_minor_gc_begin_hook = NULL;
caml_timing_hook caml_minor_gc_end_hook = NULL;
caml_timing_hook caml_finalise_begin_hook = NULL;
caml_timing_hook caml_finalise_end_hook = NULL;

#ifdef DEBUG

int caml_failed_assert (char * expr, char * file, int line)
{
  fprintf (stderr, "file %s; line %d ### Assertion failed: %s\n",
           file, line, expr);
  fflush (stderr);
  abort();
}

void caml_set_fields (value v, unsigned long start, unsigned long filler)
{
  mlsize_t i;
  for (i = start; i < Wosize_val (v); i++){
    Field (v, i) = (value) filler;
  }
}

#endif /* DEBUG */

uintnat caml_verb_gc = 0;

void caml_gc_message (int level, char *msg, uintnat arg)
{
  if ((caml_verb_gc & level) != 0){
    fprintf (stderr, msg, arg);
    fflush (stderr);
  }
}

CAMLexport void caml_fatal_error (char *msg)
{
  fprintf (stderr, "%s", msg);
  exit(2);
}

CAMLexport void caml_fatal_error_arg (char *fmt, char *arg)
{
  fprintf (stderr, fmt, arg);
  exit(2);
}

CAMLexport void caml_fatal_error_arg2 (char *fmt1, char *arg1,
                                       char *fmt2, char *arg2)
{
  fprintf (stderr, fmt1, arg1);
  fprintf (stderr, fmt2, arg2);
  exit(2);
}

/* [size] and [modulo] are numbers of bytes */
char *caml_aligned_malloc (asize_t size, int modulo, void **block)
{
  char *raw_mem;
  uintnat aligned_mem;
                                                  Assert (modulo < Page_size);
  raw_mem = (char *) malloc (size + Page_size);
  if (raw_mem == NULL) return NULL;
  *block = raw_mem;
  raw_mem += modulo;                /* Address to be aligned */
  aligned_mem = (((uintnat) raw_mem / Page_size + 1) * Page_size);
#ifdef DEBUG
  {
    uintnat *p;
    uintnat *p0 = (void *) *block,
            *p1 = (void *) (aligned_mem - modulo),
            *p2 = (void *) (aligned_mem - modulo + size),
            *p3 = (void *) ((char *) *block + size + Page_size);

    for (p = p0; p < p1; p++) *p = Debug_filler_align;
    for (p = p1; p < p2; p++) *p = Debug_uninit_align;
    for (p = p2; p < p3; p++) *p = Debug_filler_align;
  }
#endif
  return (char *) (aligned_mem - modulo);
}

/* If you change the caml_ext_table* functions, also update
   asmrun/spacetime.c:find_trie_node_from_libunwind. */

void caml_ext_table_init(struct ext_table * tbl, int init_capa)
{
  tbl->size = 0;
  tbl->capacity = init_capa;
  tbl->contents = caml_stat_alloc(sizeof(void *) * init_capa);
}

int caml_ext_table_add(struct ext_table * tbl, void * data)
{
  int res;
  if (tbl->size >= tbl->capacity) {
    tbl->capacity *= 2;
    tbl->contents =
      caml_stat_resize(tbl->contents, sizeof(void *) * tbl->capacity);
  }
  res = tbl->size;
  tbl->contents[res] = data;
  tbl->size++;
  return res;
}

void caml_ext_table_remove(struct ext_table * tbl, void * data)
{
  int i;
  for (i = 0; i < tbl->size; i++) {
    if (tbl->contents[i] == data) {
      caml_stat_free(tbl->contents[i]);
      memmove(&tbl->contents[i], &tbl->contents[i + 1],
              (tbl->size - i - 1) * sizeof(void *));
      tbl->size--;
    }
  }
}

void caml_ext_table_clear(struct ext_table * tbl, int free_entries)
{
  int i;
  if (free_entries) {
    for (i = 0; i < tbl->size; i++) caml_stat_free(tbl->contents[i]);
  }
  tbl->size = 0;
}

void caml_ext_table_free(struct ext_table * tbl, int free_entries)
{
  caml_ext_table_clear(tbl, free_entries);
  caml_stat_free(tbl->contents);
}

CAMLexport char * caml_strdup(const char * s)
{
  size_t slen = strlen(s);
  char * res = caml_stat_alloc(slen + 1);
  memcpy(res, s, slen + 1);
  return res;
}

CAMLexport char * caml_strconcat(int n, ...)
{
  va_list args;
  char * res, * p;
  size_t len;
  int i;

  len = 0;
  va_start(args, n);
  for (i = 0; i < n; i++) {
    const char * s = va_arg(args, const char *);
    len += strlen(s);
  }
  va_end(args);
  res = caml_stat_alloc(len + 1);
  va_start(args, n);
  p = res;
  for (i = 0; i < n; i++) {
    const char * s = va_arg(args, const char *);
    size_t l = strlen(s);
    memcpy(p, s, l);
    p += l;
  }
  va_end(args);
  *p = 0;
  return res;
}

/* Runtime warnings */

uintnat caml_runtime_warnings = 0;
static int caml_runtime_warnings_first = 1;

int caml_runtime_warnings_active(void)
{
  if (!caml_runtime_warnings) return 0;
  if (caml_runtime_warnings_first) {
    fprintf(stderr, "[ocaml] (use Sys.enable_runtime_warnings to control "
                    "these warnings)\n");
    caml_runtime_warnings_first = 0;
  }
  return 1;
}

#ifdef CAML_INSTR
/* Timers for profiling GC and allocation (experimental, Linux-only) */

#include <limits.h>
#include <sys/types.h>
#include <unistd.h>

struct CAML_INSTR_BLOCK *CAML_INSTR_LOG = NULL;
intnat CAML_INSTR_STARTTIME, CAML_INSTR_STOPTIME;

#define Get_time(p,i) ((p)->ts[(i)].tv_nsec + 1000000000 * (p)->ts[(i)].tv_sec)

void CAML_INSTR_INIT (void)
{
  char *s;

  CAML_INSTR_STARTTIME = 0;
  s = getenv ("OCAML_INSTR_START");
  if (s != NULL) CAML_INSTR_STARTTIME = atol (s);
  CAML_INSTR_STOPTIME = LONG_MAX;
  s = getenv ("OCAML_INSTR_STOP");
  if (s != NULL) CAML_INSTR_STOPTIME = atol (s);
}

void CAML_INSTR_ATEXIT (void)
{
  int i;
  struct CAML_INSTR_BLOCK *p, *prev, *next;
  FILE *f = NULL;
  char *fname;

  fname = caml_secure_getenv ("OCAML_INSTR_FILE");
  if (fname != NULL){
    char *mode = "a";
    char buf [1000];
    char *name = fname;

    if (name[0] == '@'){
      snprintf (buf, sizeof(buf), "%s.%d", name + 1, getpid ());
      name = buf;
    }
    if (name[0] == '+'){
      mode = "a";
      name = name + 1;
    }else if (name [0] == '>' || name[0] == '-'){
      mode = "w";
      name = name + 1;
    }
    f = fopen (name, mode);
  }

  if (f != NULL){
    /* reverse the list */
    prev = NULL;
    p = CAML_INSTR_LOG;
    while (p != NULL){
      next = p->next;
      p->next = prev;
      prev = p;
      p = next;
    }
    CAML_INSTR_LOG = prev;
    fprintf (f, "==== OCAML INSTRUMENTATION DATA %s\n", OCAML_VERSION_STRING);
    for (p = CAML_INSTR_LOG; p != NULL; p = p->next){
      for (i = 0; i < p->index; i++){
        fprintf (f, "@@ %19ld %19ld %s\n",
                 Get_time (p, i), Get_time(p, i+1), p->tag[i+1]);
      }
      if (p->tag[0][0] != '\000'){
        fprintf (f, "@@ %19ld %19ld %s\n",
                 Get_time (p, 0), Get_time(p, p->index), p->tag[0]);
      }
    }
    fclose (f);
  }
}
#endif /* CAML_INSTR */
