/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 2002 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../../LICENSE.  */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#include <sys/types.h>
#include <netdb.h>

int main(int argc, char ** argv)
{
#if NUM_ARGS == 5
  struct hostent *hp;
  struct hostent h;
  char buffer[10];
  int h_errno;
  hp = gethostbyname_r("www.caml.org", &h, buffer, 10, &h_errno);
#elif NUM_ARGS == 6
  struct hostent *hp;
  struct hostent h;
  char buffer[10];
  int h_errno;
  int rc;
  rc = gethostbyname_r("www.caml.org", &h, buffer, 10, &hp, &h_errno);
#elif NUM_ARGS == 3
  struct hostent h;
  struct hostent_data hdata;
  int rc;
  rc = gethostbyname_r("www.caml.org", &h, &hdata);
#endif
  return 0;
}
