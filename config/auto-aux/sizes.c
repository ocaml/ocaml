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

#include <limits.h>
#include <stdint.h>

#if UINT_MAX == 255ULL
#define SIZEOFINT 1
#elif UINT_MAX == 65535ULL
#define SIZEOFINT 2
#elif UINT_MAX == 4294967295ULL
#define SIZEOFINT 4
#elif UINT_MAX == 18446744073709551615ULL
#define SIZEOFINT 8
#else
#error Cannot evaluate sizeof(int)
#endif

#if ULONG_MAX == 255ULL
#define SIZEOFLONG 1
#elif ULONG_MAX == 65535ULL
#define SIZEOFLONG 2
#elif ULONG_MAX == 4294967295ULL
#define SIZEOFLONG 4
#elif ULONG_MAX == 18446744073709551615ULL
#define SIZEOFLONG 8
#else
#error Cannot evaluate sizeof(long)
#endif

#if SIZE_MAX == 255ULL
#define SIZEOFPOINTER 1
#elif SIZE_MAX == 65535ULL
#define SIZEOFPOINTER 2
#elif SIZE_MAX == 4294967295ULL
#define SIZEOFPOINTER 4
#elif SIZE_MAX == 18446744073709551615ULL
#define SIZEOFPOINTER 8
#else
#error Cannot evaluate sizeof(void *)
#endif

#if USHRT_MAX == 255ULL
#define SIZEOFSHORT 1
#elif USHRT_MAX == 65535ULL
#define SIZEOFSHORT 2
#elif USHRT_MAX == 4294967295ULL
#define SIZEOFSHORT 4
#elif USHRT_MAX == 18446744073709551615ULL
#define SIZEOFSHORT 8
#else
#error Cannot evaluate sizeof(short)
#endif

#if ULLONG_MAX == 255ULL
#define SIZEOFLONGLONG 1
#elif ULLONG_MAX == 65535ULL
#define SIZEOFLONGLONG 2
#elif ULLONG_MAX == 4294967295ULL
#define SIZEOFLONGLONG 4
#elif ULLONG_MAX == 18446744073709551615ULL
#define SIZEOFLONGLONG 8
#else
#error Cannot evaluate sizeof(long long)
#endif

SIZEOFINT SIZEOFLONG SIZEOFPOINTER SIZEOFSHORT SIZEOFLONGLONG
