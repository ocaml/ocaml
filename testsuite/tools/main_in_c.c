/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*                        David Allsopp, Tarides                          */
/*                                                                        */
/*   Copyright 2024 David Allsopp Ltd.                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

/* Small stub used by testLinkModes.ml for tests where the main program is in C,
   rather than OCaml. */

#define CAML_INTERNALS
#include <caml/callback.h>

int main_os(int argc, char_os **argv)
{
  caml_startup(argv);
  caml_shutdown();
  return 0;
}
