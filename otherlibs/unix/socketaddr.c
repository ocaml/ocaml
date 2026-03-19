/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 1996 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#include <string.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <errno.h>
#include "caml/unixsupport.h"

#ifdef HAS_SOCKETS

#include "caml/socketaddr.h"
#undef caml_unix_get_sockaddr
#undef caml_unix_alloc_sockaddr

#ifdef _WIN32
#undef EAFNOSUPPORT
#define EAFNOSUPPORT WSAEAFNOSUPPORT
#include <io.h>
#endif

CAMLexport value caml_unix_alloc_inet_addr(struct in_addr * a)
{
  /* Use a string rather than an abstract block so that it can be
     marshaled safely.  Remember that a is in network byte order,
     hence is marshaled in an endian-independent manner. */
  return caml_alloc_initialized_string(4, (char *)a);
}

#ifdef HAS_IPV6

CAMLexport value caml_unix_alloc_inet6_addr(struct in6_addr * a)
{
  return caml_alloc_initialized_string(16, (char *)a);
}

#endif

void caml_unix_get_sockaddr(value vaddr,
                       struct sockaddr_storage * addr /*out*/,
                       socklen_t * addr_len /*out*/)
{
  switch(Tag_val(vaddr)) {
  case 0:                       /* ADDR_UNIX */
    { value path;
      mlsize_t len;
      int is_path;
      struct sockaddr_un *s_unix = (struct sockaddr_un *) addr;
      path = Field(vaddr, 0);
      len = caml_string_length(path);
      s_unix->sun_family = AF_UNIX;
      if (len >= sizeof(s_unix->sun_path)) {
        caml_unix_error(ENAMETOOLONG, "", path);
      }
      /* Pathname sockets have a non-empty name not starting with NUL, see
         unix(7). */
      is_path = Byte(path, 0) != 0;
      if (is_path && ! caml_string_is_c_safe(path)) {
        caml_unix_error(ENOENT, "", path);
      }
      memmove(s_unix->sun_path, String_val(path), len + 1);
      /* Pathname sockets should include the trailing NUL, see unix(7). */
      *addr_len =
        offsetof(struct sockaddr_un, sun_path) + len + (is_path ? 1 : 0);
      break;
    }
  case 1:                       /* ADDR_INET */
#ifdef HAS_IPV6
    if (caml_string_length(Field(vaddr, 0)) == 16) {
      struct sockaddr_in6 *s_inet6 = (struct sockaddr_in6 *) addr;
      memset(s_inet6, 0, sizeof(struct sockaddr_in6));
      s_inet6->sin6_family = AF_INET6;
      s_inet6->sin6_addr = GET_INET6_ADDR(Field(vaddr, 0));
      s_inet6->sin6_port = htons(Int_val(Field(vaddr, 1)));
#ifdef SIN6_LEN
      s_inet6->sin6_len = sizeof(struct sockaddr_in6);
#endif
      *addr_len = sizeof(struct sockaddr_in6);
    } else {
#endif
      struct sockaddr_in *s_inet = (struct sockaddr_in *) addr;
      memset(s_inet, 0, sizeof(struct sockaddr_in));
      s_inet->sin_family = AF_INET;
      s_inet->sin_addr = GET_INET_ADDR(Field(vaddr, 0));
      s_inet->sin_port = htons(Int_val(Field(vaddr, 1)));
#ifdef SIN6_LEN
      s_inet->sin_len = sizeof(struct sockaddr_in);
#endif
      *addr_len = sizeof(struct sockaddr_in);
    }
    break;
  }
}

static value alloc_unix_sockaddr(value path) {
  CAMLparam1(path);
  CAMLlocal1(res);
  res = caml_alloc_small(1, 0);
  Field(res,0) = path;
  CAMLreturn(res);
}

value caml_unix_alloc_sockaddr(struct sockaddr * addr /*in*/,
                          socklen_t addr_len, int close_on_error)
{
  CAMLparam0();
  CAMLlocal1(a);
  value res;
  if (addr_len < offsetof(struct sockaddr, sa_data)) {
    // Only possible for an unnamed AF_UNIX socket, in
    // which case sa_family might be uninitialized.
    CAMLreturn(alloc_unix_sockaddr(caml_alloc_string(0)));
  }

  switch(addr->sa_family) {
  case AF_UNIX:
    { struct sockaddr_un *s_unix = (struct sockaddr_un *) addr;
      /* Based on recommendation in section BUGS of Linux unix(7). See
         https://man7.org/linux/man-pages/man7/unix.7.html#BUGS. */
      size_t struct_offset = offsetof(struct sockaddr_un, sun_path);
      size_t path_length = 0;
      if (addr_len > struct_offset) {
        path_length = addr_len - struct_offset;

        /* paths _may_ be null-terminated, but Linux abstract sockets
         * start with a null, and may contain internal nulls. */
        path_length = (
#if defined(__linux__) || defined(__HAIKU__)
          (s_unix->sun_path[0] == '\0') ? path_length :
#endif
          strnlen(s_unix->sun_path, path_length)
        );
      }

      res = alloc_unix_sockaddr(
        caml_alloc_initialized_string(path_length, s_unix->sun_path));
      break;
    }
  case AF_INET:
    { struct sockaddr_in *s_inet = (struct sockaddr_in *) addr;
      a = caml_unix_alloc_inet_addr(&s_inet->sin_addr);
      res = caml_alloc_small(2, 1);
      Field(res,0) = a;
      Field(res,1) = Val_int(ntohs(s_inet->sin_port));
      break;
    }
#ifdef HAS_IPV6
  case AF_INET6:
    { struct sockaddr_in6 *s_inet6 = (struct sockaddr_in6 *) addr;
      a = caml_unix_alloc_inet6_addr(&s_inet6->sin6_addr);
      res = caml_alloc_small(2, 1);
      Field(res,0) = a;
      Field(res,1) = Val_int(ntohs(s_inet6->sin6_port));
      break;
    }
#endif
  default:
    if (close_on_error != -1) {
#ifdef _WIN32
      closesocket (close_on_error);
#else
      close (close_on_error);
#endif
    }
    caml_unix_error(EAFNOSUPPORT, "", Nothing);
  }
  CAMLreturn(res);
}

#endif
