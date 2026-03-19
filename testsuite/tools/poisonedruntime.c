/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*            David Allsopp, University of Cambridge & Tarides            */
/*                                                                        */
/*   Copyright 2025 David Allsopp Ltd.                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

/* Micro-program used to sit in PATH to test local path search for bytecode
   executables. */

#define CAML_INTERNALS
#include <caml/callback.h>
#include <stdio.h>

int main_os(int argc, char_os **argv)
{
  printf("The poisoned runtime has been invoked!\n"
         "This suggests something is wrong in stdlib/header.c\n");
  return 1;
}
