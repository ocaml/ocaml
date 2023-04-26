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

#include "caml/codefrag.h"
#include "caml/lf_skiplist.h"
#include "caml/md5.h"
#include "caml/memory.h"
#include "caml/misc.h"
#include <stdatomic.h>
#include <stddef.h>
#include <string.h>

struct code_fragment_garbage {
  struct code_fragment *cf;
  struct code_fragment_garbage *next;
};

static struct code_fragment_garbage *_Atomic garbage_head = NULL;

static struct lf_skiplist code_fragments_by_pc;

static struct lf_skiplist code_fragments_by_num;

static int _Atomic code_fragments_counter = 1;

void caml_init_codefrag(void) {
  caml_lf_skiplist_init(&code_fragments_by_pc);
  caml_lf_skiplist_init(&code_fragments_by_num);
}

int caml_register_code_fragment(char *start, char *end,
                                enum digest_status digest_kind,
                                unsigned char *opt_digest) {
  struct code_fragment *cf = caml_stat_alloc(sizeof(struct code_fragment));

  cf->code_start = start;
  cf->code_end = end;
  switch (digest_kind) {
  case DIGEST_LATER:
    break;
  case DIGEST_NOW:
    /* no one knows of this code fragment yet, no need to take its lock */
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
  cf->fragnum = atomic_fetch_add_explicit
                  (&code_fragments_counter, 1, memory_order_relaxed);
  caml_plat_mutex_init(&cf->mutex);
  caml_lf_skiplist_insert(&code_fragments_by_pc, (uintnat)start, (uintnat)cf);
  caml_lf_skiplist_insert(&code_fragments_by_num, (uintnat)cf->fragnum,
                          (uintnat)cf);
  return cf->fragnum;
}

static void caml_free_code_fragment(struct code_fragment *cf) {
  caml_plat_mutex_free(&cf->mutex);
  caml_stat_free(cf);
}

void caml_remove_code_fragment(struct code_fragment *cf) {
  struct code_fragment_garbage *cf_cell;

  caml_lf_skiplist_remove(&code_fragments_by_pc, (uintnat)cf->code_start);

  /* This is conditional on remove returning success because it's possible
    for [caml_remove_code_fragment] to be called concurrently and we need
    to ensure that only one code_fragment is put on to the garbage list */
  if (caml_lf_skiplist_remove(&code_fragments_by_num, cf->fragnum)) {
    cf_cell = (struct code_fragment_garbage *)caml_stat_alloc(
        sizeof(struct code_fragment_garbage));

    cf_cell->cf = cf;

    do {
      cf_cell->next = atomic_load_explicit(&garbage_head, memory_order_acquire);
    } while (!atomic_compare_exchange_strong(&garbage_head, &cf_cell->next,
                                             cf_cell));
  }
}

struct code_fragment *caml_find_code_fragment_by_pc(char *pc) {
  struct code_fragment *cf;
  uintnat key, data;

  if (caml_lf_skiplist_find_below(&code_fragments_by_pc, (uintnat)pc, &key,
                                  &data)) {
    cf = (struct code_fragment *)data;
    CAMLassert(cf->code_start <= pc);
    if (pc < cf->code_end)
      return cf;
  }
  return NULL;
}

struct code_fragment *caml_find_code_fragment_by_num(int fragnum) {
  uintnat data;
  if (caml_lf_skiplist_find(&code_fragments_by_num, fragnum, &data)) {
    return (struct code_fragment *)data;
  } else {
    return NULL;
  }
}

unsigned char *caml_digest_of_code_fragment(struct code_fragment *cf) {
  unsigned char *digest;

  /* Note: this approach is a bit heavy-handed as we take a lock in
     all cases. It would be possible to take a lock only in the
     DIGEST_LATER case, which occurs at most once per fragment, by
     using double-checked locking -- see #11791. */
  caml_plat_lock(&cf->mutex);
  {
    if (cf->digest_status == DIGEST_IGNORE) {
      digest = NULL;
    } else if (cf->digest_status == DIGEST_LATER) {
      caml_md5_block(cf->digest, cf->code_start, cf->code_end - cf->code_start);
      cf->digest_status = DIGEST_PROVIDED;
      digest = cf->digest;
    } else {
      digest = cf->digest;
    }
  }
  caml_plat_unlock(&cf->mutex);

  return digest;
}

struct code_fragment *
caml_find_code_fragment_by_digest(unsigned char digest[16]) {
  FOREACH_LF_SKIPLIST_ELEMENT(e, &code_fragments_by_pc, {
    struct code_fragment *cf = (struct code_fragment *)e->data;
    unsigned char *d = caml_digest_of_code_fragment(cf);
    if (d != NULL && memcmp(digest, d, 16) == 0)
      return cf;
  })
  return NULL;
}

/* This is only ever called from a stw by one domain */
void caml_code_fragment_cleanup (void)
{
  struct code_fragment_garbage *curr;

  caml_lf_skiplist_free_garbage(&code_fragments_by_pc);
  caml_lf_skiplist_free_garbage(&code_fragments_by_num);

  curr = atomic_load_explicit(&garbage_head, memory_order_acquire);

  while (curr != NULL) {
    struct code_fragment_garbage *next = curr->next;

    caml_free_code_fragment(curr->cf);
    caml_stat_free(curr);

    curr = next;
  }

  atomic_store_explicit(&garbage_head, NULL, memory_order_release);
}
