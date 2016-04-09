/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*   Contributed by Stephane Glondu <steph@glondu.net>                    */
/*                                                                        */
/*   Copyright 2009 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#include <errno.h>

#include <sys/types.h>
#include <limits.h>
#include <grp.h>

int main(void)
{
  gid_t gidset[1];
  gidset[0] = 0;
  if (setgroups(1, gidset) == -1 && errno != EPERM) return 1;
  return 0;
}
