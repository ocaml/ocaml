/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*             Damien Doligez, projet Para, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1998 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License.         */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#include "main.h"

/* Allocate from application memory or from Multifinder memory;
   always leave at least kMinimumMemory free in application memory.
*/
OSErr AllocHandle (Size size, Handle *result)
{
  OSErr err;

  if (FreeMem () >= size + kMinimumMemory){
    *result = NewHandle (size);
    err = MemError ();
  }
  if (err != noErr) *result = TempNewHandle (size, &err);
  return err;
}
