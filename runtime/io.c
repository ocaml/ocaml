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

#define CAML_INTERNALS

/* Buffered input/output. */

#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <string.h>
#include <stdio.h>
#include <sys/types.h>
#include "caml/config.h"
#ifdef HAS_UNISTD
#include <unistd.h>
#endif
#ifdef __CYGWIN__
#include </usr/include/io.h>
#endif
#include "caml/alloc.h"
#include "caml/custom.h"
#include "caml/fail.h"
#include "caml/io.h"
#include "caml/memory.h"
#include "caml/misc.h"
#include "caml/mlvalues.h"
#include "caml/osdeps.h"
#include "caml/signals.h"
#include "caml/sys.h"

#ifndef SEEK_SET
#define SEEK_SET 0
#define SEEK_CUR 1
#define SEEK_END 2
#endif

#if defined(_WIN32)
#include <io.h>
#define lseek _lseeki64
#endif


/* Hooks for locking channels */

CAMLexport void (*caml_channel_mutex_free) (struct channel *) = NULL;
CAMLexport void (*caml_channel_mutex_lock) (struct channel *) = NULL;
CAMLexport void (*caml_channel_mutex_unlock) (struct channel *) = NULL;
CAMLexport void (*caml_channel_mutex_unlock_exn) (void) = NULL;

/* List of opened channels */
CAMLexport struct channel * caml_all_opened_channels = NULL;

/* Basic functions over type struct channel *.
   These functions can be called directly from C.
   No locking is performed. */

/* Functions shared between input and output */

static void check_pending(struct channel *channel)
{
  if (caml_check_pending_actions()) {
    /* Temporarily unlock the channel, to ensure locks are not held
       while any signal handlers (or finalisers, etc) are running */
    Unlock(channel);
    caml_process_pending_actions();
    Lock(channel);
  }
}

Caml_inline int descriptor_is_in_binary_mode(int fd)
{
#if defined(_WIN32) || defined(__CYGWIN__)
  int oldmode = setmode(fd, O_TEXT);
  if (oldmode != -1 && oldmode != O_TEXT) setmode(fd, oldmode);
  return oldmode == O_BINARY;
#else
  return 1;
#endif
}

CAMLexport struct channel * caml_open_descriptor_in(int fd)
{
  struct channel * channel;

  channel = (struct channel *) caml_stat_alloc(sizeof(struct channel));
  channel->fd = fd;
  caml_enter_blocking_section_no_pending();
  channel->offset = lseek(fd, 0, SEEK_CUR);
  caml_leave_blocking_section();
  channel->curr = channel->max = channel->buff;
  channel->end = channel->buff + IO_BUFFER_SIZE;
  channel->mutex = NULL;
  channel->revealed = 0;
  channel->old_revealed = 0;
  channel->refcount = 0;
  channel->flags = descriptor_is_in_binary_mode(fd) ? 0 : CHANNEL_TEXT_MODE;
  channel->next = caml_all_opened_channels;
  channel->prev = NULL;
  channel->name = NULL;
  if (caml_all_opened_channels != NULL)
    caml_all_opened_channels->prev = channel;
  caml_all_opened_channels = channel;
  return channel;
}

CAMLexport struct channel * caml_open_descriptor_out(int fd)
{
  struct channel * channel;

  channel = caml_open_descriptor_in(fd);
  channel->max = NULL;
  return channel;
}

static void unlink_channel(struct channel *channel)
{
  if (channel->prev == NULL) {
    CAMLassert (channel == caml_all_opened_channels);
    caml_all_opened_channels = caml_all_opened_channels->next;
    if (caml_all_opened_channels != NULL)
      caml_all_opened_channels->prev = NULL;
  } else {
    channel->prev->next = channel->next;
    if (channel->next != NULL) channel->next->prev = channel->prev;
  }
}

CAMLexport void caml_close_channel(struct channel *channel)
{
  close(channel->fd);
  if (channel->refcount > 0) return;
  if (caml_channel_mutex_free != NULL) (*caml_channel_mutex_free)(channel);
  unlink_channel(channel);
  caml_stat_free(channel->name);
  caml_stat_free(channel);
}

CAMLexport file_offset caml_channel_size(struct channel *channel)
{
  file_offset here, end;
  int fd;

  check_pending(channel);
  /* We extract data from [channel] before dropping the OCaml lock, in case
     someone else touches the block. */
  fd = channel->fd;
  here = channel->flags & CHANNEL_TEXT_MODE ? -1 : channel->offset;
  caml_enter_blocking_section_no_pending();
  if (here == -1) {
    here = lseek(fd, 0, SEEK_CUR);
    if (here == -1) goto error;
  }
  end = lseek(fd, 0, SEEK_END);
  if (end == -1) goto error;
  if (lseek(fd, here, SEEK_SET) != here) goto error;
  caml_leave_blocking_section();
  return end;
 error:
  caml_leave_blocking_section();
  caml_sys_error(NO_ARG);
}

CAMLexport int caml_channel_binary_mode(struct channel *channel)
{
  return channel->flags & CHANNEL_TEXT_MODE ? 0 : 1;
}

/* Output */

/* Attempt to flush the buffer. This will make room in the buffer for
   at least one character. Returns true if the buffer is empty at the
   end of the flush, or false if some data remains in the buffer.
 */

CAMLexport int caml_flush_partial(struct channel *channel)
{
  int towrite, written;
 again:
  check_pending(channel);

  towrite = channel->curr - channel->buff;
  CAMLassert (towrite >= 0);
  if (towrite > 0) {
    written = caml_write_fd(channel->fd, channel->flags,
                            channel->buff, towrite);
    if (written == Io_interrupted) goto again;
    channel->offset += written;
    if (written < towrite)
      memmove(channel->buff, channel->buff + written, towrite - written);
    channel->curr -= written;
  }
  return (channel->curr == channel->buff);
}

/* Flush completely the buffer. */

CAMLexport void caml_flush(struct channel *channel)
{
  while (! caml_flush_partial(channel)) /*nothing*/;
}

/* Output data */

CAMLexport void caml_putword(struct channel *channel, uint32_t w)
{
  if (! caml_channel_binary_mode(channel))
    caml_failwith("output_binary_int: not a binary channel");
  caml_putch(channel, w >> 24);
  caml_putch(channel, w >> 16);
  caml_putch(channel, w >> 8);
  caml_putch(channel, w);
}

CAMLexport int caml_putblock(struct channel *channel, char *p, intnat len)
{
  int n, free;

  n = len >= INT_MAX ? INT_MAX : (int) len;
  free = channel->end - channel->curr;
  if (n < free) {
    /* Write request small enough to fit in buffer: transfer to buffer. */
    memmove(channel->curr, p, n);
    channel->curr += n;
    return n;
  } else {
    /* Write request overflows buffer (or just fills it up): transfer whatever
       fits to buffer and write the buffer */
    memmove(channel->curr, p, free);
    channel->curr = channel->end;
    caml_flush_partial(channel);
    return free;
  }
}

CAMLexport void caml_really_putblock(struct channel *channel,
                                     char *p, intnat len)
{
  int written;
  while (len > 0) {
    written = caml_putblock(channel, p, len);
    p += written;
    len -= written;
  }
}

CAMLexport void caml_seek_out(struct channel *channel, file_offset dest)
{
  caml_flush(channel);
  caml_enter_blocking_section_no_pending();
  if (lseek(channel->fd, dest, SEEK_SET) != dest) {
    caml_leave_blocking_section();
    caml_sys_error(NO_ARG);
  }
  caml_leave_blocking_section();
  channel->offset = dest;
}

CAMLexport file_offset caml_pos_out(struct channel *channel)
{
  return channel->offset + (file_offset)(channel->curr - channel->buff);
}

/* Input */

int caml_do_read(int fd, char *p, unsigned int n)
{
  int r;
  do {
    r = caml_read_fd(fd, 0, p, n);
  } while (r == Io_interrupted);
  return r;
}

CAMLexport unsigned char caml_refill(struct channel *channel)
{
  int n;
 again:
  check_pending(channel);
  n = caml_read_fd(channel->fd, channel->flags,
                   channel->buff, channel->end - channel->buff);
  if (n == Io_interrupted) goto again;
  else if (n == 0) caml_raise_end_of_file();
  channel->offset += n;
  channel->max = channel->buff + n;
  channel->curr = channel->buff + 1;
  return (unsigned char)(channel->buff[0]);
}

CAMLexport uint32_t caml_getword(struct channel *channel)
{
  int i;
  uint32_t res;

  if (! caml_channel_binary_mode(channel))
    caml_failwith("input_binary_int: not a binary channel");
  res = 0;
  for(i = 0; i < 4; i++) {
    res = (res << 8) + caml_getch(channel);
  }
  return res;
}

CAMLexport int caml_getblock(struct channel *channel, char *p, intnat len)
{
  int n, avail, nread;
 again:
  check_pending(channel);
  n = len >= INT_MAX ? INT_MAX : (int) len;
  avail = channel->max - channel->curr;
  if (n <= avail) {
    memmove(p, channel->curr, n);
    channel->curr += n;
    return n;
  } else if (avail > 0) {
    memmove(p, channel->curr, avail);
    channel->curr += avail;
    return avail;
  } else {
    nread = caml_read_fd(channel->fd, channel->flags, channel->buff,
                         channel->end - channel->buff);
    if (nread == Io_interrupted) goto again;
    channel->offset += nread;
    channel->max = channel->buff + nread;
    if (n > nread) n = nread;
    memmove(p, channel->buff, n);
    channel->curr = channel->buff + n;
    return n;
  }
}

/* Returns the number of bytes read. */
CAMLexport intnat caml_really_getblock(struct channel *chan, char *p, intnat n)
{
  intnat k = n;
  int r;
  while (k > 0) {
    r = caml_getblock(chan, p, k);
    if (r == 0) break;
    p += r;
    k -= r;
  }
  return n - k;
}

CAMLexport void caml_seek_in(struct channel *channel, file_offset dest)
{
  if (dest >= channel->offset - (channel->max - channel->buff)
      && dest <= channel->offset
      && (channel->flags & CHANNEL_TEXT_MODE) == 0) {
    channel->curr = channel->max - (channel->offset - dest);
  } else {
    caml_enter_blocking_section_no_pending();
    if (lseek(channel->fd, dest, SEEK_SET) != dest) {
      caml_leave_blocking_section();
      caml_sys_error(NO_ARG);
    }
    caml_leave_blocking_section();
    channel->offset = dest;
    channel->curr = channel->max = channel->buff;
  }
}

CAMLexport file_offset caml_pos_in(struct channel *channel)
{
  return channel->offset - (file_offset)(channel->max - channel->curr);
}

intnat caml_input_scan_line(struct channel *channel)
{
  char * p;
  int n;
 again:
  check_pending(channel);
  p = channel->curr;
  do {
    if (p >= channel->max) {
      /* No more characters available in the buffer */
      if (channel->curr > channel->buff) {
        /* Try to make some room in the buffer by shifting the unread
           portion at the beginning */
        memmove(channel->buff, channel->curr, channel->max - channel->curr);
        n = channel->curr - channel->buff;
        channel->curr -= n;
        channel->max -= n;
        p -= n;
      }
      if (channel->max >= channel->end) {
        /* Buffer is full, no room to read more characters from the input.
           Return the number of characters in the buffer, with negative
           sign to indicate that no newline was encountered. */
        return -(channel->max - channel->curr);
      }
      /* Fill the buffer as much as possible */
      n = caml_read_fd(channel->fd, channel->flags,
                       channel->max, channel->end - channel->max);
      if (n == Io_interrupted) goto again;
      else if (n == 0) {
        /* End-of-file encountered. Return the number of characters in the
           buffer, with negative sign since we haven't encountered
           a newline. */
        return -(channel->max - channel->curr);
      }
      channel->offset += n;
      channel->max += n;
    }
  } while (*p++ != '\n');
  /* Found a newline. Return the length of the line, newline included. */
  return (p - channel->curr);
}

/* OCaml entry points for the I/O functions.  Wrap struct channel *
   objects into a heap-allocated object.  Perform locking
   and unlocking around the I/O operations. */

void caml_finalize_channel(value vchan)
{
  struct channel * chan = Channel(vchan);
  if ((chan->flags & CHANNEL_FLAG_MANAGED_BY_GC) == 0) return;
  if (--chan->refcount > 0) return;
  if (caml_channel_mutex_free != NULL) (*caml_channel_mutex_free)(chan);

  if (chan->fd != -1 && chan->name && caml_runtime_warnings_active())
    fprintf(stderr,
            "[ocaml] channel opened on file '%s' dies without being closed\n",
            chan->name
            );

  if (chan->max == NULL && chan->curr != chan->buff){
    /*
      This is an unclosed out channel (chan->max == NULL) with a
      non-empty buffer: keep it around so the OCaml [at_exit] function
      gets a chance to flush it.  We would want to simply flush the
      channel now, but (i) flushing can raise exceptions, and (ii) it
      is potentially a blocking operation.  Both are forbidden in a
      finalization function.

      Refs:
      http://caml.inria.fr/mantis/view.php?id=6902
      https://github.com/ocaml/ocaml/pull/210
    */
    if (chan->name && caml_runtime_warnings_active())
      fprintf(stderr,
              "[ocaml] (moreover, it has unflushed data)\n"
              );
  } else {
    unlink_channel(chan);
    caml_stat_free(chan->name);
    caml_stat_free(chan);
  }
}

static int compare_channel(value vchan1, value vchan2)
{
  struct channel * chan1 = Channel(vchan1);
  struct channel * chan2 = Channel(vchan2);
  return (chan1 == chan2) ? 0 : (chan1 < chan2) ? -1 : 1;
}

static intnat hash_channel(value vchan)
{
  return (intnat) (Channel(vchan));
}

static struct custom_operations channel_operations = {
  "_chan",
  caml_finalize_channel,
  compare_channel,
  hash_channel,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default,
  custom_fixed_length_default
};

CAMLexport value caml_alloc_channel(struct channel *chan)
{
  value res;
  chan->refcount++;             /* prevent finalization during next alloc */
  res = caml_alloc_custom_mem(&channel_operations, sizeof(struct channel *),
                              sizeof(struct channel));
  Channel(res) = chan;
  return res;
}

CAMLprim value caml_ml_open_descriptor_in(value fd)
{
  struct channel * chan = caml_open_descriptor_in(Int_val(fd));
  chan->flags |= CHANNEL_FLAG_MANAGED_BY_GC;
  return caml_alloc_channel(chan);
}

CAMLprim value caml_ml_open_descriptor_out(value fd)
{
  struct channel * chan = caml_open_descriptor_out(Int_val(fd));
  chan->flags |= CHANNEL_FLAG_MANAGED_BY_GC;
  return caml_alloc_channel(chan);
}

CAMLprim value caml_ml_set_channel_name(value vchannel, value vname)
{
  struct channel * channel = Channel(vchannel);
  caml_stat_free(channel->name);
  if (caml_string_length(vname) > 0)
    channel->name = caml_stat_strdup(String_val(vname));
  else
    channel->name = NULL;
  return Val_unit;
}

#define Pair_tag 0

CAMLprim value caml_ml_out_channels_list (value unit)
{
  CAMLparam0 ();
  CAMLlocal3 (res, tail, chan);
  struct channel * channel;

  res = Val_emptylist;
  for (channel = caml_all_opened_channels;
       channel != NULL;
       channel = channel->next)
    /* Testing channel->fd >= 0 looks unnecessary, as
       caml_ml_close_channel changes max when setting fd to -1. */
    if (channel->max == NULL) {
      chan = caml_alloc_channel (channel);
      tail = res;
      res = caml_alloc_small (2, Pair_tag);
      Field (res, 0) = chan;
      Field (res, 1) = tail;
    }
  CAMLreturn (res);
}

CAMLprim value caml_channel_descriptor(value vchannel)
{
  int fd = Channel(vchannel)->fd;
  if (fd == -1) { errno = EBADF; caml_sys_error(NO_ARG); }
  return Val_int(fd);
}

CAMLprim value caml_ml_close_channel(value vchannel)
{
  int result;
  int do_syscall;
  int fd;

  /* For output channels, must have flushed before */
  struct channel * channel = Channel(vchannel);
  if (channel->fd != -1){
    fd = channel->fd;
    channel->fd = -1;
    do_syscall = 1;
  }else{
    do_syscall = 0;
    result = 0;
  }
  /* Ensure that every read or write on the channel will cause an
     immediate caml_flush_partial or caml_refill, thus raising a Sys_error
     exception */
  channel->curr = channel->max = channel->end;

  if (do_syscall) {
    caml_enter_blocking_section_no_pending();
    result = close(fd);
    caml_leave_blocking_section();
  }

  if (result == -1) caml_sys_error (NO_ARG);
  return Val_unit;
}

/* EOVERFLOW is the Unix98 error indicating that a file position or file
   size is not representable.
   ERANGE is the ANSI C error indicating that some argument to some
   function is out of range.  This is less precise than EOVERFLOW,
   but guaranteed to be defined on all ANSI C environments. */
#ifndef EOVERFLOW
#define EOVERFLOW ERANGE
#endif

static file_offset ml_channel_size(value vchannel)
{
  CAMLparam1 (vchannel);
  struct channel * channel = Channel(vchannel);
  file_offset size;

  Lock(channel);
  size = caml_channel_size(Channel(vchannel));
  Unlock(channel);
  CAMLreturnT(file_offset, size);
}

CAMLprim value caml_ml_channel_size(value vchannel)
{
  file_offset size = ml_channel_size(vchannel);
  if (size > Max_long) { errno = EOVERFLOW; caml_sys_error(NO_ARG); }
  return Val_long(size);
}

CAMLprim value caml_ml_channel_size_64(value vchannel)
{
  return Val_file_offset(ml_channel_size(vchannel));
}

CAMLprim value caml_ml_set_binary_mode(value vchannel, value mode)
{
#if defined(_WIN32) || defined(__CYGWIN__)
  struct channel * channel = Channel(vchannel);
#if defined(_WIN32)
  /* The implementation of [caml_read_fd] and [caml_write_fd] in win32.c
     doesn't support socket I/O with CRLF conversion. */
  if ((channel->flags & CHANNEL_FLAG_FROM_SOCKET) != 0
      && ! Bool_val(mode)) {
    errno = EINVAL;
    caml_sys_error(NO_ARG);
  }
#endif
  if (setmode(channel->fd, Bool_val(mode) ? O_BINARY : O_TEXT) == -1)
    caml_sys_error(NO_ARG);
  if (Bool_val(mode))
    channel->flags &= ~CHANNEL_TEXT_MODE;
  else
    channel->flags |= CHANNEL_TEXT_MODE;
#endif
  return Val_unit;
}

/*
   If the channel is closed, DO NOT raise a "bad file descriptor"
   exception, but do nothing (the buffer is already empty).
   This is because some libraries will flush at exit, even on
   file descriptors that may be closed.
*/

CAMLprim value caml_ml_flush(value vchannel)
{
  CAMLparam1 (vchannel);
  struct channel * channel = Channel(vchannel);

  if (channel->fd == -1) CAMLreturn(Val_unit);
  Lock(channel);
  caml_flush(channel);
  Unlock(channel);
  CAMLreturn (Val_unit);
}

CAMLprim value caml_ml_output_char(value vchannel, value ch)
{
  CAMLparam2 (vchannel, ch);
  struct channel * channel = Channel(vchannel);

  Lock(channel);
  caml_putch(channel, Long_val(ch));
  Unlock(channel);
  CAMLreturn (Val_unit);
}

CAMLprim value caml_ml_output_int(value vchannel, value w)
{
  CAMLparam2 (vchannel, w);
  struct channel * channel = Channel(vchannel);

  Lock(channel);
  caml_putword(channel, (uint32_t) Long_val(w));
  Unlock(channel);
  CAMLreturn (Val_unit);
}

CAMLprim value caml_ml_output_bytes(value vchannel, value buff, value start,
                              value length)
{
  CAMLparam4 (vchannel, buff, start, length);
  struct channel * channel = Channel(vchannel);
  intnat pos = Long_val(start);
  intnat len = Long_val(length);

  Lock(channel);
    /* We cannot call caml_really_putblock here because buff may move
       during caml_write_fd */
    while (len > 0) {
      int written = caml_putblock(channel, &Byte(buff, pos), len);
      pos += written;
      len -= written;
    }
  Unlock(channel);
  CAMLreturn (Val_unit);
}

CAMLprim value caml_ml_output(value vchannel, value buff, value start,
                              value length)
{
  return caml_ml_output_bytes (vchannel, buff, start, length);
}

CAMLprim value caml_ml_seek_out(value vchannel, value pos)
{
  CAMLparam2 (vchannel, pos);
  struct channel * channel = Channel(vchannel);

  Lock(channel);
  caml_seek_out(channel, Long_val(pos));
  Unlock(channel);
  CAMLreturn (Val_unit);
}

CAMLprim value caml_ml_seek_out_64(value vchannel, value pos)
{
  CAMLparam2 (vchannel, pos);
  struct channel * channel = Channel(vchannel);

  Lock(channel);
  caml_seek_out(channel, File_offset_val(pos));
  Unlock(channel);
  CAMLreturn (Val_unit);
}

CAMLprim value caml_ml_pos_out(value vchannel)
{
  file_offset pos = caml_pos_out(Channel(vchannel));
  if (pos > Max_long) { errno = EOVERFLOW; caml_sys_error(NO_ARG); }
  return Val_long(pos);
}

CAMLprim value caml_ml_pos_out_64(value vchannel)
{
  return Val_file_offset(caml_pos_out(Channel(vchannel)));
}

CAMLprim value caml_ml_input_char(value vchannel)
{
  CAMLparam1 (vchannel);
  struct channel * channel = Channel(vchannel);
  unsigned char c;

  Lock(channel);
  c = caml_getch(channel);
  Unlock(channel);
  CAMLreturn (Val_long(c));
}

CAMLprim value caml_ml_input_int(value vchannel)
{
  CAMLparam1 (vchannel);
  struct channel * channel = Channel(vchannel);
  intnat i;

  Lock(channel);
  i = caml_getword(channel);
  Unlock(channel);
#ifdef ARCH_SIXTYFOUR
  i = (i << 32) >> 32;          /* Force sign extension */
#endif
  CAMLreturn (Val_long(i));
}

CAMLprim value caml_ml_input(value vchannel, value buff, value vstart,
                             value vlength)
{
  CAMLparam4 (vchannel, buff, vstart, vlength);
  struct channel * channel = Channel(vchannel);
  intnat start, len;
  int n, avail, nread;

  Lock(channel);
 again:
  check_pending(channel);
  /* We cannot call caml_getblock here because buff may move during
     caml_read_fd */
  start = Long_val(vstart);
  len = Long_val(vlength);
  n = len >= INT_MAX ? INT_MAX : (int) len;
  avail = channel->max - channel->curr;
  if (n <= avail) {
    memmove(&Byte(buff, start), channel->curr, n);
    channel->curr += n;
  } else if (avail > 0) {
    memmove(&Byte(buff, start), channel->curr, avail);
    channel->curr += avail;
    n = avail;
  } else {
    nread = caml_read_fd(channel->fd, channel->flags, channel->buff,
                         channel->end - channel->buff);
    if (nread == Io_interrupted) goto again;
    channel->offset += nread;
    channel->max = channel->buff + nread;
    if (n > nread) n = nread;
    memmove(&Byte(buff, start), channel->buff, n);
    channel->curr = channel->buff + n;
  }
  Unlock(channel);
  CAMLreturn (Val_long(n));
}

CAMLprim value caml_ml_seek_in(value vchannel, value pos)
{
  CAMLparam2 (vchannel, pos);
  struct channel * channel = Channel(vchannel);

  Lock(channel);
  caml_seek_in(channel, Long_val(pos));
  Unlock(channel);
  CAMLreturn (Val_unit);
}

CAMLprim value caml_ml_seek_in_64(value vchannel, value pos)
{
  CAMLparam2 (vchannel, pos);
  struct channel * channel = Channel(vchannel);

  Lock(channel);
  caml_seek_in(channel, File_offset_val(pos));
  Unlock(channel);
  CAMLreturn (Val_unit);
}

CAMLprim value caml_ml_pos_in(value vchannel)
{
  file_offset pos = caml_pos_in(Channel(vchannel));
  if (pos > Max_long) { errno = EOVERFLOW; caml_sys_error(NO_ARG); }
  return Val_long(pos);
}

CAMLprim value caml_ml_pos_in_64(value vchannel)
{
  return Val_file_offset(caml_pos_in(Channel(vchannel)));
}

CAMLprim value caml_ml_input_scan_line(value vchannel)
{
  CAMLparam1 (vchannel);
  struct channel * channel = Channel(vchannel);
  intnat res;

  Lock(channel);
  res = caml_input_scan_line(channel);
  Unlock(channel);
  CAMLreturn (Val_long(res));
}

CAMLprim value caml_terminfo_rows(value vchannel)
{
  return Val_int(caml_num_rows_fd(Channel(vchannel)->fd));
}
