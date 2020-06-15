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

static struct skiplist code_fragments_by_pc = SKIPLIST_STATIC_INITIALIZER;

static struct skiplist code_fragments_by_num = SKIPLIST_STATIC_INITIALIZER;

static int code_fragments_counter = 0;

int caml_register_code_fragment(char * start, char * end,
                                enum digest_status digest_kind,
                                unsigned char * opt_digest)
{
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
  cf->fragnum = code_fragments_counter++;
  caml_skiplist_insert(&code_fragments_by_pc,
                       (uintnat) start, (uintnat) cf);
  caml_skiplist_insert(&code_fragments_by_num,
                       (uintnat) cf->fragnum, (uintnat) cf);
  return cf->fragnum;
}

void caml_remove_code_fragment(struct code_fragment * cf)
{
  caml_skiplist_remove(&code_fragments_by_pc, (uintnat) cf->code_start);
  caml_skiplist_remove(&code_fragments_by_num, cf->fragnum);
  caml_stat_free(cf);
}

struct code_fragment * caml_find_code_fragment_by_pc(char *pc)
{
  struct code_fragment * cf;
  uintnat key, data;

  if (caml_skiplist_find_below(&code_fragments_by_pc,
                               (uintnat) pc, &key, &data)) {
    cf = (struct code_fragment *) data;
    CAMLassert(cf->code_start <= pc);
    if (pc < cf->code_end) return cf;
  }
  return NULL;
}

struct code_fragment * caml_find_code_fragment_by_num(int fragnum)
{
  uintnat data;
  if (caml_skiplist_find(&code_fragments_by_num, fragnum, &data)) {
    return (struct code_fragment *) data;
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
  FOREACH_SKIPLIST_ELEMENT(e, &code_fragments_by_pc, {
    struct code_fragment * cf = (struct code_fragment *) e->data;
    unsigned char * d = caml_digest_of_code_fragment(cf);
    if (d != NULL && memcmp(digest, d, 16) == 0) return cf;
  })
  return NULL;
}
