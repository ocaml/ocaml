/***********************************************************************/
/*                                                                     */
/*                         Caml Special Light                          */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1995 Institut National de Recherche en Informatique et   */
/*  Automatique.  Distributed only by permission.                      */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

char * runtime_name = "cslrun";
char * errmsg = "Cannot exec cslrun.\n";

int main(argc, argv)
     int argc;
     char ** argv;
{
  execvp(runtime_name, argv);
  write(2, errmsg, strlen(errmsg));
  return 2;
}
