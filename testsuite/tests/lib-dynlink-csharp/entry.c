/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*                        Alain Frisch, LexiFi                         */
/*                                                                     */
/*  Copyright 2007 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the Q Public License version 1.0.               */
/*                                                                     */
/***********************************************************************/

#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/mlvalues.h>
#include <caml/callback.h>
#include <caml/custom.h>
#include <caml/fail.h>

__declspec(dllexport) void __stdcall start_caml_engine() {
  char * argv[2];
  argv[0] = "--";
  argv[1] = NULL;
  caml_startup(argv);
}
