/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  Automatique.  Distributed only by permission.                      */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

char * runtime_name = "ocamlrun";
char * errmsg = "Cannot exec ocamlrun.\n";

int main(argc, argv)
     int argc;
     char ** argv;
{
  execvp(runtime_name, argv);
  write(2, errmsg, strlen(errmsg));
  return 2;
}
