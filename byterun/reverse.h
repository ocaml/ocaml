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

/* Swap byte-order in 32-bit integers and in 64-bit floats */

#ifndef _reverse_
#define _reverse_

#define Reverse_int32(w) {                                                    \
  char * _p;                                                                  \
  int _a;                                                                     \
  _p = (char *) (w);                                                          \
  _a = _p[0];                                                                 \
  _p[0] = _p[3];                                                              \
  _p[3] = _a;                                                                 \
  _a = _p[1];                                                                 \
  _p[1] = _p[2];                                                              \
  _p[2] = _a;                                                                 \
}

#define Reverse_double(d) {                                                   \
  char * _p;                                                                  \
  int _a;                                                                     \
  _p = (char *) (d);                                                          \
  _a = _p[0];                                                                 \
  _p[0] = _p[7];                                                              \
  _p[7] = _a;                                                                 \
  _a = _p[1];                                                                 \
  _p[1] = _p[6];                                                              \
  _p[6] = _a;                                                                 \
  _a = _p[2];                                                                 \
  _p[2] = _p[5];                                                              \
  _p[5] = _a;                                                                 \
  _a = _p[3];                                                                 \
  _p[3] = _p[4];                                                              \
  _p[4] = _a;                                                                 \
}

#endif /* _reverse_ */
