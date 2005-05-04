/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with   */
/*  the special exception on linking described in file ../../LICENSE. */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#include <mlvalues.h>
#include <alloc.h>
#include <fail.h>
#include "unixsupport.h"

#ifdef HAS_SOCKETS

#include <sys/time.h>
#include <sys/types.h>
#include <sys/socket.h>

#include "socketaddr.h"

#ifndef SO_DEBUG
#define SO_DEBUG (-1)
#endif
#ifndef SO_BROADCAST
#define SO_BROADCAST (-1)
#endif
#ifndef SO_REUSEADDR
#define SO_REUSEADDR (-1)
#endif
#ifndef SO_KEEPALIVE
#define SO_KEEPALIVE (-1)
#endif
#ifndef SO_DONTROUTE
#define SO_DONTROUTE (-1)
#endif
#ifndef SO_OOBINLINE
#define SO_OOBINLINE (-1)
#endif
#ifndef SO_ACCEPTCONN
#define SO_ACCEPTCONN (-1)
#endif
#ifndef SO_SNDBUF
#define SO_SNDBUF (-1)
#endif
#ifndef SO_RCVBUF
#define SO_RCVBUF (-1)
#endif
#ifndef SO_ERROR
#define SO_ERROR (-1)
#endif
#ifndef SO_TYPE
#define SO_TYPE (-1)
#endif
#ifndef SO_RCVLOWAT
#define SO_RCVLOWAT (-1)
#endif
#ifndef SO_SNDLOWAT
#define SO_SNDLOWAT (-1)
#endif
#ifndef SO_LINGER
#define SO_LINGER (-1)
#endif
#ifndef SO_RCVTIMEO
#define SO_RCVTIMEO (-1)
#endif
#ifndef SO_SNDTIMEO
#define SO_SNDTIMEO (-1)
#endif

static int sockopt_bool[] = {
  SO_DEBUG, SO_BROADCAST, SO_REUSEADDR, SO_KEEPALIVE,
  SO_DONTROUTE, SO_OOBINLINE, SO_ACCEPTCONN };

static int sockopt_int[] = {
  SO_SNDBUF, SO_RCVBUF, SO_ERROR, SO_TYPE, SO_RCVLOWAT, SO_SNDLOWAT };

static int sockopt_optint[] = { SO_LINGER };

static int sockopt_float[] = { SO_RCVTIMEO, SO_SNDTIMEO };

CAMLexport value getsockopt_int(int *sockopt, value socket,
                                int level, value option)
{
  int optval;
  socklen_param_type optsize;

  optsize = sizeof(optval);
  if (getsockopt(Int_val(socket), level, sockopt[Int_val(option)],
                 (void *) &optval, &optsize) == -1)
    uerror("getsockopt", Nothing);
  return Val_int(optval);
}

CAMLexport value setsockopt_int(int *sockopt, value socket, int level,
                                value option, value status)
{
  int optval = Int_val(status);
  if (setsockopt(Int_val(socket), level, sockopt[Int_val(option)],
                 (void *) &optval, sizeof(optval)) == -1)
    uerror("setsockopt", Nothing);
  return Val_unit;
}

CAMLprim value unix_getsockopt_bool(value socket, value option) {
  value res = getsockopt_int(sockopt_bool, socket, SOL_SOCKET, option);
  return Val_bool(Int_val(res));
}

CAMLprim value unix_setsockopt_bool(value socket, value option, value status)
{
 return setsockopt_int(sockopt_bool, socket, SOL_SOCKET, option, status);
}

CAMLprim value unix_getsockopt_int(value socket, value option) {
  return getsockopt_int(sockopt_int, socket, SOL_SOCKET, option);
}

CAMLprim value unix_setsockopt_int(value socket, value option, value status)
{
 return setsockopt_int(sockopt_int, socket, SOL_SOCKET, option, status);
}

CAMLexport value getsockopt_optint(int *sockopt, value socket,
                                   int level, value option)
{
  struct linger optval;
  socklen_param_type optsize;
  value res = Val_int(0);                       /* None */

  optsize = sizeof(optval);
  if (getsockopt(Int_val(socket), level, sockopt[Int_val(option)],
                 (void *) &optval, &optsize) == -1)
    uerror("getsockopt_optint", Nothing);
  if (optval.l_onoff != 0) {
    res = alloc_small(1, 0);
    Field(res, 0) = Val_int(optval.l_linger);
  }
  return res;
}

CAMLexport value setsockopt_optint(int *sockopt, value socket, int level,
                                   value option, value status)
{
  struct linger optval;

  optval.l_onoff = Is_block (status);
  if (optval.l_onoff)
    optval.l_linger = Int_val (Field (status, 0));
  if (setsockopt(Int_val(socket), level, sockopt[Int_val(option)],
                 (void *) &optval, sizeof(optval)) == -1)
    uerror("setsockopt_optint", Nothing);
  return Val_unit;
}

CAMLprim value unix_getsockopt_optint(value socket, value option)
{
  return getsockopt_optint(sockopt_optint, socket, SOL_SOCKET, option);
}

CAMLprim value unix_setsockopt_optint(value socket, value option, value status)
{
  return setsockopt_optint(sockopt_optint, socket, SOL_SOCKET, option, status);
}

CAMLexport value getsockopt_float(int *sockopt, value socket,
                                  int level, value option)
{
  struct timeval tv;
  socklen_param_type optsize;

  optsize = sizeof(tv);
  if (getsockopt(Int_val(socket), level, sockopt[Int_val(option)],
                 (void *) &tv, &optsize) == -1)
    uerror("getsockopt_float", Nothing);
  return copy_double((double) tv.tv_sec + (double) tv.tv_usec / 1e6);
}

CAMLexport value setsockopt_float(int *sockopt, value socket, int level,
                                  value option, value status)
{
  struct timeval tv;
  double tv_f;

  tv_f = Double_val(status);
  tv.tv_sec = (int)tv_f;
  tv.tv_usec = (int) (1e6 * (tv_f - tv.tv_sec));
  if (setsockopt(Int_val(socket), level, sockopt[Int_val(option)],
                 (void *) &tv, sizeof(tv)) == -1)
    uerror("setsockopt_float", Nothing);
  return Val_unit;
}

CAMLprim value unix_getsockopt_float(value socket, value option)
{
  return getsockopt_float(sockopt_float, socket, SOL_SOCKET, option);
}

CAMLprim value unix_setsockopt_float(value socket, value option, value status)
{
  return setsockopt_float(sockopt_float, socket, SOL_SOCKET, option, status);
}

#else

CAMLprim value unix_getsockopt_bool(value socket, value option)
{ invalid_argument("getsockopt not implemented"); }

CAMLprim value unix_setsockopt_bool(value socket, value option, value status)
{ invalid_argument("setsockopt not implemented"); }

CAMLprim value unix_getsockopt_int(value socket, value option)
{ invalid_argument("getsockopt_int not implemented"); }

CAMLprim value unix_setsockopt_int(value socket, value option, value status)
{ invalid_argument("setsockopt_int not implemented"); }

CAMLprim value unix_getsockopt_optint(value socket, value option)
{ invalid_argument("getsockopt_optint not implemented"); }

CAMLprim value unix_setsockopt_optint(value socket, value option, value status)
{ invalid_argument("setsockopt_optint not implemented"); }

CAMLprim value unix_getsockopt_float(value socket, value option)
{ invalid_argument("getsockopt_float not implemented"); }

CAMLprim value unix_setsockopt_float(value socket, value option, value status)
{ invalid_argument("setsockopt_float not implemented"); }

#endif
