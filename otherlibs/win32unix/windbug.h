/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*  Contributed by Sylvain Le Gall for Lexifi                          */
/*                                                                     */
/*  Copyright 2008 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../../LICENSE.  */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

/*#define DBUG*/

#ifdef DBUG

/* Initialize and cleanup dbug variable */
void dbug_init    (void);
void dbug_cleanup (void);

/* Test if we are in dbug mode */
int  dbug_test    (void);

/* Print if we are in dbug mode */
void dbug_print (const char * fmt, ...);

#define DBUG_INIT    dbug_init()
#define DBUG_CLEANUP dbug_cleanup()

#else
#define DBUG_INIT
#define DBUG_CLEANUP
#endif

