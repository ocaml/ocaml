/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Damien Doligez, projet Para, INRIA Rocquencourt          */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  Automatique.  Distributed only by permission.                      */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

/* Cursor rotation for MPW tools (ocamlrun and ocamlyacc) */

#ifndef _rotatecursor_
#define _rotatecursor_

/* [*p1] and [*p2] will be set to 1 when the time comes to call 
   [ui_periodic_action].  If p1 or p2 is not used, pass it as NULL.
*/
void rotatecursor_init (int volatile *p1, int volatile *p2);

/* [reverse] is 0 to rotate the cursor clockwise, anything else to
   rotate counterclockwise.  This function always returns 0.
 */
int rotatecursor_action (int reverse);

#endif /* _rotatecursor_ */
