/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*             Damien Doligez, projet Para, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1998 Institut National de Recherche en Informatique et   */
/*  en Automatique.  Distributed only by permission                    */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#include "main.h"

void LocalToGlobalRect (Rect *r)
{
  Point *p = (Point *) r;

  LocalToGlobal (&p[0]);
  LocalToGlobal (&p[1]);
}
