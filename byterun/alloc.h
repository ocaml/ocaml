/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*         Xavier Leroy and Damien Doligez, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  Automatique.  Distributed only by permission.                      */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#ifndef _alloc_
#define _alloc_


#include "misc.h"
#include "mlvalues.h"

value alloc (mlsize_t, tag_t);
value alloc_tuple (mlsize_t);
value alloc_string (mlsize_t);
value alloc_final (mlsize_t, final_fun, mlsize_t, mlsize_t);
value copy_string (char *);
value copy_string_array (char **);
value copy_double (double);
value alloc_array (value (*funct) (char *), char ** array);
int convert_flag_list (value, int *);


#endif /* _alloc_ */
