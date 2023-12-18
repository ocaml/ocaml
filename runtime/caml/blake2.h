/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*           Xavier Leroy, Collège de France and Inria Paris              */
/*                                                                        */
/*   Copyright 2022 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

/* BLAKE2 message digest */

#ifndef CAML_BLAKE2_H
#define CAML_BLAKE2_H

#ifdef CAML_INTERNALS

#include "misc.h"

#define BLAKE2_BLOCKSIZE 128

struct BLAKE2_context {
  uint64_t h[8];
  uint64_t len[2];
  size_t numbytes;
  unsigned char buffer[BLAKE2_BLOCKSIZE];
};

CAMLextern void
caml_BLAKE2Init(struct BLAKE2_context * context,
                size_t hashlen, size_t keylen, const unsigned char * key);
CAMLextern void
caml_BLAKE2Update(struct BLAKE2_context * context,
                  const unsigned char * data, size_t len);
CAMLextern void
caml_BLAKE2Final(struct BLAKE2_context * context,
                 size_t hashlen, unsigned char * hash);

#endif /* CAML_INTERNALS */

#endif /* CAML_BLAKE2_H */
