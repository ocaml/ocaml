/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*              Damien Doligez, projet Para, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 1996 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#ifndef CAML_GC_H
#define CAML_GC_H


#include "mlvalues.h"

/* This depends on the layout of the header.  See [mlvalues.h]. */

#define Make_header_with_reserved(wosize, tag, color, reserved)      \
      (/*CAMLassert ((wosize) <= Max_wosize),*/                      \
       ((header_t) (Hd_reserved(reserved))                           \
                    + ((header_t) (wosize) << HEADER_WOSIZE_SHIFT)   \
                    + (color) /* colors are pre-shifted */           \
                    + (tag_t) (tag)))


#define Make_header(wosize, tag, color) \
        Make_header_with_reserved(wosize, tag, color, 0)

#endif /* CAML_GC_H */
