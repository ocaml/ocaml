/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*            Anil Madhavapeddy, University of Cambridge                  */
/*                                                                        */
/*   Copyright 2022 Anil Madhavapeddy                                     */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#define _GNU_SOURCE
#include <string.h>
#include <stdio.h>

#ifndef _WIN32
static __thread char caml_errbuf[1024];
#endif

char*
caml_strerror(int errnum)
{
#if defined(__GLIBC__) /* GNU variant */
    return strerror_r(errnum, caml_errbuf, sizeof(caml_errbuf));
#elif defined(_WIN32)  /* Windows has a thread-safe strerror */
    return strerror(errnum);
#else                  /* POSIX variant */
    int res = strerror_r(errnum, caml_errbuf, sizeof(caml_errbuf));
    /* glibc<2.13 returns -1/sets errno, >2.13 returns +ve errno.
       We cannot get ERANGE since buffer size is large enough, so
       only possible error is EINVAL. */
    if (res != 0) {
        snprintf(caml_errbuf, sizeof(caml_errbuf), "Unknown error %d", errnum);
    }
    return caml_errbuf;
#endif
}
