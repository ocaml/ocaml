/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*         Manuel Serrano and Xavier Leroy, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 2000 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License.         */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#ifndef _custom_
#define _custom_


#include "mlvalues.h"

struct custom_operations {
  char *identifier;
  void (*finalize)(value v);
  int (*compare)(value v1, value v2);
  long (*hash)(value v);
  void (*serialize)(value v, 
                    /*out*/ unsigned long * wsize_32 /*size in bytes*/,
                    /*out*/ unsigned long * wsize_64 /*size in bytes*/);
  unsigned long (*deserialize)(void * dst);
};

#define custom_finalize_default NULL
extern int custom_compare_default(value v1, value v2);
#define custom_hash_default NULL
extern void custom_serialize_default(value v, unsigned long * wsize_32,
                                     unsigned long * wsize_64);
#define custom_deserialize_default NULL

#define Custom_ops_val(v) (*((struct custom_operations **) (v)))

extern void register_custom_operations(struct custom_operations * ops);
extern struct custom_operations * find_custom_operations(char * ident);

#endif
