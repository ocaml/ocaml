/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

/* Swap byte-order in 16, 32, and 64-bit integers or floats */

#ifndef _reverse_
#define _reverse_

#define Reverse_16(dst,src) {                                               \
  char * _p, * _q;                                                          \
  char _a;                                                                  \
  _p = (char *) (src);                                                      \
  _q = (char *) (dst);                                                      \
  _a = _p[0];                                                               \
  _q[0] = _p[1];                                                            \
  _q[1] = _a;                                                               \
}

#define Reverse_32(dst,src) {                                               \
  char * _p, * _q;                                                          \
  char _a, _b;                                                              \
  _p = (char *) (src);                                                      \
  _q = (char *) (dst);                                                      \
  _a = _p[0];                                                               \
  _b = _p[1];                                                               \
  _q[0] = _p[3];                                                            \
  _q[1] = _p[2];                                                            \
  _q[3] = _a;                                                               \
  _q[2] = _b;                                                               \
}

#define Reverse_64(dst,src) {                                               \
  char * _p, * _q;                                                          \
  char _a, _b;                                                              \
  _p = (char *) (src);                                                      \
  _q = (char *) (dst);                                                      \
  _a = _p[0];                                                               \
  _b = _p[1];                                                               \
  _q[0] = _p[7];                                                            \
  _q[1] = _p[6];                                                            \
  _q[7] = _a;                                                               \
  _q[6] = _b;                                                               \
  _a = _p[2];                                                               \
  _b = _p[3];                                                               \
  _q[2] = _p[5];                                                            \
  _q[3] = _p[4];                                                            \
  _q[5] = _a;                                                               \
  _q[4] = _b;                                                               \
}


#endif /* _reverse_ */
