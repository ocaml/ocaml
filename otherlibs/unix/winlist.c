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

/* Basic list function in C. */

#include "winlist.h"
#include <windows.h>

void caml_win32_list_init (LPLIST lst)
{
  lst->lpNext = NULL;
}

void caml_win32_list_cleanup (LPLIST lst)
{
  lst->lpNext = NULL;
}

void caml_win32_list_next_set (LPLIST lst, LPLIST next)
{
  lst->lpNext = next;
}

LPLIST caml_win32_list_next (LPLIST lst)
{
  return lst->lpNext;
}

int caml_win32_list_length (LPLIST lst)
{
  int length = 0;
  LPLIST iter = lst;
  while (iter != NULL)
  {
    length++;
    iter = caml_win32_list_next(iter);
  };
  return length;
}

LPLIST caml_win32_list_concat (LPLIST lsta, LPLIST lstb)
{
  LPLIST res = NULL;
  LPLIST iter = NULL;
  LPLIST iterPrev = NULL;

  if (lsta == NULL)
  {
    res = lstb;
  }
  else if (lstb == NULL)
  {
    res = lsta;
  }
  else
  {
    res = lsta;
    iter = lsta;
    while (iter != NULL)
    {
      iterPrev = iter;
      iter = caml_win32_list_next(iter);
    };
    iterPrev->lpNext = lstb;
  };

  return res;
}
