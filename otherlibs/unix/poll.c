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

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/signals.h>
#include "unixsupport.h"

#ifdef HAS_POLL
#ifdef HAS_POLL_H
#include <poll.h>
#endif

static inline int int_of_event(value event) {
  switch Int_val(event) {
    case 0:
      return POLLIN;
    case 1:
      return POLLRDNORM;
    case 2:
      return POLLRDBAND;
    case 3:
      return POLLPRI;
    case 4:
      return POLLOUT;
    case 5:
      return POLLWRNORM;
    case 6:
      return POLLWRBAND;
    case 7:
      return POLLERR;
    case 8:
      return POLLHUP;
    case 9:
      return POLLNVAL;
    default:
      return -1;
  }
}

static inline void populate_pollfd(struct pollfd *fds, value fdlist) {
  value fd, event;
  int pos;

  Begin_roots2(fd, event);
    pos = 0;
    for (fd = fdlist; fd != Val_int(0); fd = Field(fd, 1)) {
      fds[pos].fd = Int_val(Field(Field(fd,0),0));
      fds[pos].events = 0;
      for (event = Field(Field(fd,0),1); event != Val_int(0); event = Field(event, 1))
        fds[pos].events |= int_of_event(Field(event,0));
      pos++;
    }
  End_roots();
}

static inline value list_prepend(value l, value v) {
  value newres;

  Begin_roots3(newres, l, v);
    newres = caml_alloc_small(2, 0);
    Field(newres, 0) = v;
    Field(newres, 1) = l;
    l = newres;
  End_roots();

  return l;
}

static inline value extract_fd_events(int event) {
  value res = Val_int(0);

  Begin_roots1(res);
    if (event & POLLIN)
      res = list_prepend(res,Val_int(0));
    if (event & POLLRDNORM)
      res = list_prepend(res,Val_int(1));
    if (event & POLLRDBAND)
      res = list_prepend(res,Val_int(2));
    if (event & POLLPRI)
      res = list_prepend(res,Val_int(3));
    if (event & POLLOUT)
      res = list_prepend(res,Val_int(4));
    if (event & POLLWRNORM)
      res = list_prepend(res,Val_int(5));
    if (event & POLLWRBAND)
      res = list_prepend(res,Val_int(6));
    if (event & POLLERR)
      res = list_prepend(res,Val_int(7));
    if (event & POLLHUP)
      res = list_prepend(res,Val_int(8));
    if (event & POLLNVAL)
      res = list_prepend(res,Val_int(9));
  End_roots();
  return res;
}

static value extract_ready_fds(struct pollfd *fds, nfds_t nfds) {
  nfds_t n;
  value res = Val_int(0);

  Begin_roots1(res);
    for (n = 0; n < nfds; n++) {
      value elem = caml_alloc_small(2,0);
      Field(elem,0) = Val_int(fds[n].fd);
      Field(elem,1) = extract_fd_events(fds[n].revents);
      res = list_prepend(res,elem);
    }
  End_roots();
  return res;
}

CAMLprim value unix_poll(value fdlist, value timeout)
{
  struct pollfd *fds;
  nfds_t nfds = 0;
  int tm, retcode;
  value fd, res;

  Begin_roots1 (fdlist);
    for (fd = fdlist; fd != Val_int(0); fd = Field(fd, 1))
      nfds++;

    fds = malloc(nfds*sizeof(struct pollfd));
    if (fds == NULL) caml_raise_out_of_memory();

    populate_pollfd(fds,fdlist);

    tm = Double_val(timeout)*1000;
    if (tm < 0.0) tm = -1;

    caml_enter_blocking_section();
    retcode = poll(fds, nfds, tm);
    caml_leave_blocking_section();

    if (retcode == -1) {
      free(fds);
      uerror("poll", Nothing);
    }

    res = extract_ready_fds(fds,nfds);

  End_roots();
  return res;
}

#else

CAMLprim value unix_poll(value fds, value timeout)
{ caml_invalid_argument("poll not implemented"); }

#endif
