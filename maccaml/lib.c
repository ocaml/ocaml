/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*             Damien Doligez, projet Para, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1998 Institut National de Recherche en Informatique et   */
/*  en Automatique.  Distributed only by permission.                   */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#include "main.h"

/* These are declared in TextUtils.h but not implemented in Apple's
   libraries ?!
*/

void CopyPascalStringToC (ConstStr255Param src, char *dst)
{
  strncpy (dst, (char *) src + 1, src[0]);
  dst [src[0]] = '\000';
}

void CopyCStringToPascal (const char *src, Str255 dst)
{
  int l = strlen (src);

  l = l > 255 ? 255 : l;
  dst [0] = l;
  strncpy ((char *) dst + 1, src, l);
}
