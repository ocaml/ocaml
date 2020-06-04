/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Xavier Leroy, projet Cambium, INRIA Paris                  */
/*                                                                        */
/*   Copyright 2020 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#define CAML_INTERNALS

/* A table of all code fragments (main program and dynlinked modules) */

#include <string.h>
#include <stddef.h>
#include "caml/codefrag.h"
#include "caml/misc.h"
#include "caml/md5.h"
#include "caml/memory.h"
#include "caml/skiplist.h"

static struct skiplist caml_code_fragments_list = SKIPLIST_STATIC_INITIALIZER;

static struct ext_table caml_code_fragments_table = {0, 0, NULL};

int caml_register_code_fragment(char * start, char * end,
                                enum digest_status digest_kind,
                                unsigned char * opt_digest)
{
  int pos;
  struct code_fragment * cf = caml_stat_alloc(sizeof(struct code_fragment));

  cf->code_start = start;
  cf->code_end = end;
  switch (digest_kind) {
  case DIGEST_LATER:
    break;
  case DIGEST_NOW:
    caml_md5_block(cf->digest, cf->code_start, cf->code_end - cf->code_start);
    digest_kind = DIGEST_PROVIDED;
    break;
  case DIGEST_PROVIDED:
    memcpy(cf->digest, opt_digest, 16);
    break;
  case DIGEST_IGNORE:
    break;
  }
  cf->digest_status = digest_kind;
  caml_skiplist_insert(&caml_code_fragments_list,
                       (uintnat) start, (uintnat) cf);
  if (caml_code_fragments_table.capacity == 0) {
    caml_ext_table_init(&caml_code_fragments_table, 8);
  }
  pos = caml_ext_table_add(&caml_code_fragments_table, cf);
  cf->fragnum = pos;
  return pos;
}

void caml_remove_code_fragment(struct code_fragment * cf)
{
  struct code_fragment * c;
  int i;

  caml_skiplist_remove(&caml_code_fragments_list, (uintnat) cf->code_start);
  /* Remove cf from the table, maintaining the invariant that
     if c is the i-th element of the table, c->fragnum is equal to i. */
  for (i = cf->fragnum; i < caml_code_fragments_table.size - 1; i++) {
    c = caml_code_fragments_table.contents[i + 1];
    caml_code_fragments_table.contents[i] = c;
    c->fragnum = i;
  }
  caml_code_fragments_table.size--;
  caml_stat_free(cf);
}

struct code_fragment * caml_find_code_fragment_by_pc(char *pc)
{
  struct code_fragment * cf;
  uintnat key, data;

  if (caml_skiplist_find_below(&caml_code_fragments_list,
                               (uintnat) pc, &key, &data)) {
    cf = (struct code_fragment *) data;
    if (cf->code_start <= pc && pc < cf->code_end) return cf;
  }
  return NULL;
}

struct code_fragment * caml_find_code_fragment_by_num(int fragnum)
{
  if (0 <= fragnum && fragnum < caml_code_fragments_table.size) {
    return caml_code_fragments_table.contents[fragnum];
  } else {
    return NULL;
  }
}

unsigned char * caml_digest_of_code_fragment(struct code_fragment * cf)
{
  if (cf->digest_status == DIGEST_IGNORE)
    return NULL;
  if (cf->digest_status == DIGEST_LATER) {
    caml_md5_block(cf->digest, cf->code_start, cf->code_end - cf->code_start);
    cf->digest_status = DIGEST_PROVIDED;
  }
  return cf->digest;
}

struct code_fragment *
   caml_find_code_fragment_by_digest(unsigned char digest[16])
{
  int i;

  for (i = caml_code_fragments_table.size - 1; i >= 0; i--) {
    struct code_fragment * cf = caml_code_fragments_table.contents[i];
    unsigned char * d = caml_digest_of_code_fragment(cf);
    if (d != NULL && memcmp(digest, d, 16) == 0) return cf;
  }
  return NULL;
}
