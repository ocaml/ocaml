/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*  Contributed by Stefan Hellermann <stefan@the2masters.de>           */
/*                                                                     */
/*  Copyright 2015 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../../LICENSE.  */
/*                                                                     */
/***********************************************************************/

// Try to find the right includes
#if defined(__linux__) || defined(__CYGWIN__)
#  define _BSD_SOURCE
#  define _DEFAULT_SOURCE
#  include <endian.h>
#elif defined(__APPLE__)
#  include <libkern/OSByteOrder.h>
#elif defined(BSD)
#   include <sys/endian.h>
#elif defined(_WIN16) || defined(_WIN32) || defined(_WIN64)
#  include <winsock2.h>
#  include <sys/param.h>
#endif

#if defined(__BIG_ENDIAN__) || \
  defined(__BYTE_ORDER__) && __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__ || \
  defined(__BYTE_ORDER) && __BYTE_ORDER == __BIG_ENDIAN || \
  defined(BYTE_ORDER) && BYTE_ORDER == BIG_ENDIAN
0
#elif defined(__LITTLE_ENDIAN__) || \
  defined(__BYTE_ORDER__) && __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__ || \
  defined(__BYTE_ORDER) && __BYTE_ORDER == __LITTLE_ENDIAN || \
  defined(BYTE_ORDER) && BYTE_ORDER == LITTLE_ENDIAN
1
#else
#  error Cannot detect endianess of target
#endif
