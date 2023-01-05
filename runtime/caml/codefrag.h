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

/* A table of all code fragments (main program and dynlinked modules) */

#ifndef CAML_CODEFRAG_H
#define CAML_CODEFRAG_H

#ifdef CAML_INTERNALS

#include "platform.h"

enum digest_status {
  DIGEST_LATER,    /* computed on demand */
  DIGEST_NOW,      /* computed by caml_register_code_fragment */
  DIGEST_PROVIDED, /* passed by caller of caml_register_code_fragment */
  DIGEST_IGNORE    /* this code fragment is private and cannot be
                      identified by its digest */
};

struct code_fragment {
  char *code_start;
  char *code_end;
  int fragnum;
  enum digest_status digest_status;
  unsigned char digest[16];
  /* the digest is obtained by hashing
     the bytes between [code_start] and [code_end]. */
  caml_plat_mutex mutex;
  /* [mutex] protects the fields [digest_status] and [digest],
     which may be written to when computing a digest
     on-demand (DIGEST_LATER) and thus require synchronization. */
};

/* Initialise codefrag. This must be done before any of the other
   operations in codefrag. */
void caml_init_codefrag(void);

/* Register a code fragment for addresses [start] (included)
   to [end] (excluded).  This range of addresses is assumed
   disjoint from all currently-registered code fragments.

   [digest_kind] explains what digest is to be associated to the code
   fragment.  If [digest_kind == DIGEST_PROVIDED], the [opt_digest]
   parameter points to the 16-byte digest of the code.
   For all other values of [digest_kind], [opt_digest] is ignored
   and should be [NULL].

   The returned integer is the fragment number (fragnum) associated
   with the new code fragment. */
extern int caml_register_code_fragment(char * start, char * end,
                                       enum digest_status digest_kind,
                                       unsigned char * opt_digest);

/* Un-register a code fragment. */
extern void caml_remove_code_fragment(struct code_fragment * cf);

/* Find the code fragment whose range of addresses contains [pc].
   Returns NULL if none exists. */
extern struct code_fragment * caml_find_code_fragment_by_pc(char *pc);

/* Find the code fragment whose fragment number is [fragnum].
   Returns NULL if none exists. */
extern struct code_fragment * caml_find_code_fragment_by_num(int fragnum);

/* Find the code fragment whose digest is equal to the given digest.
   Returns NULL if none exists. */
extern struct code_fragment *
   caml_find_code_fragment_by_digest(unsigned char digest[16]);

/* Return the digest of the given code fragment.
   If the code fragment was registered in [DIGEST_LATER] mode
   and if the digest was not computed yet, it is obtained by hashing
   the bytes between [code_start] and [code_end].
   Returns NULL if the code fragment was registered with [DIGEST_IGNORE]. */
extern unsigned char * caml_digest_of_code_fragment(struct code_fragment *);

/* Cleans up (and frees) removed code fragments. Must be called from a stop the
   world pause by only a single thread. */
extern void caml_code_fragment_cleanup(void);

#endif

#endif
