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

WindowPtr clip_window = NULL;

/* Open clipboard window or bring it to the front. */
void ClipShow (void)
{
  if (clip_window != NULL){
    SelectWindow (clip_window);
  }else{
    XXX ();
  }
}

void ClipClose (void)
{
  XXX ();
}

void ClipChanged (void)
{
  if (clip_window != NULL){
    XXX ();
  }
}
