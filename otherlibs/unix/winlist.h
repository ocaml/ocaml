/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*   Contributed by Sylvain Le Gall for Lexifi                            */
/*                                                                        */
/*   Copyright 2008 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#ifndef _WINLIST_H
#define _WINLIST_H

/* Basic list function in C. */

/* Singly-linked list data structure.
 * To transform a C struct into a list structure, you must include
 * at first position of your C struct a "LIST lst" and call caml_win32_list_init
 * on this data structure.
 *
 * See winworker.c for example.
 */
typedef struct _LIST LIST;
typedef LIST *LPLIST;

struct _LIST {
  LPLIST lpNext;
};

/* Initialize list data structure */
void caml_win32_list_init (LPLIST lst);

/* Cleanup list data structure */
void caml_win32_list_cleanup (LPLIST lst);

/* Set next element */
void caml_win32_list_next_set (LPLIST lst, LPLIST next);

/* Return next element */
LPLIST caml_win32_list_next (LPLIST);

#define LIST_NEXT(T, e) ((T)(caml_win32_list_next((LPLIST)(e))))

/* Get the number of elements */
int caml_win32_list_length (LPLIST);

/* Concatenate two lists */
LPLIST caml_win32_list_concat (LPLIST, LPLIST);

#endif /* _WINLIST_H */
