/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Damien Doligez, projet Para, INRIA Rocquencourt          */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License.         */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

/* Function declarations for non-Unix user interfaces */

#include "config.h"

void ui_exit (int return_code);
int ui_read (int file_desc, char *buf, unsigned int length);
int ui_write (int file_desc, char *buf, unsigned int length);
void ui_print_stderr (char *format, void *arg);
