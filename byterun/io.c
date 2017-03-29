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

CAMLexport struct channel * caml_open_descriptor_in(int fd)
{
  struct channel * channel;

  channel = (struct channel *) caml_stat_alloc(sizeof(struct channel));
  channel->fd = fd;
  caml_enter_blocking_section();
  channel->offset = lseek(fd, 0, SEEK_CUR);
  caml_leave_blocking_section();
  channel->curr = channel->max = channel->buff;
  channel->end = channel->buff + IO_BUFFER_SIZE;
  channel->mutex = NULL;
  channel->revealed = 0;
  channel->old_revealed = 0;
  channel->refcount = 0;
  channel->flags = 0;
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

CAMLnoreturn_start
static void raise_error(io_result)
CAMLnoreturn_end;

static void raise_error(io_result err)
{
  CAMLassert(err);
  caml_process_pending_signals();
  errno = err;
  caml_sys_error(NO_ARG);
}

static int check_retry(struct channel* chan, io_result err)
{
  if (err == EINTR) {
    /* interrupted by signal, retry */
    Unlock(chan);
    caml_process_pending_signals();
    Lock(chan);
    return 1;
  } else if (err) {
    /* operation failed, raise */
    Unlock(chan);
    raise_error(err);
  } else if (caml_signals_are_pending) {
    /* the operation succeeded, but a signal arrived since */
    Unlock(chan);
    caml_process_pending_signals();
    Lock(chan);
    return 0;
  } else {
    /* success, don't retry */
    return 0;
  }
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
  CAML_SYS_CLOSE(channel->fd);
  if (channel->refcount > 0) return;
  if (caml_channel_mutex_free != NULL) (*caml_channel_mutex_free)(channel);
  unlink_channel(channel);
  caml_stat_free(channel->name);
  caml_stat_free(channel);
  return;
}

static io_result caml_lseek_end(int fd, file_offset* end)
{
  caml_enter_blocking_section();
  *end = lseek(fd, 0, SEEK_END);
  caml_leave_blocking_section_nosig();
  if (*end == -1) return errno;
  return 0;
}

static io_result caml_lseek_set(int fd, file_offset offset)
{
  file_offset off;
  off = lseek(fd, offset, SEEK_SET);
  if (off == -1) {
    return errno;
  } else if (off != offset) {
    return EIO;
  } else {
    return 0;
  }
}

CAMLexport file_offset caml_channel_size(struct channel *channel)
{
  file_offset end;
  io_result err;

  Lock(channel);
  do {
    err = caml_lseek_end(channel->fd, &end);
  } while (check_retry(channel, err));
  do {
    err = caml_lseek_set(channel->fd, channel->offset);
  } while (check_retry(channel, err));
  Unlock(channel);

  return end;
}

CAMLexport int caml_channel_binary_mode(struct channel *channel)
{
#if defined(_WIN32) || defined(__CYGWIN__)
  int oldmode = setmode(channel->fd, O_BINARY);
  if (oldmode == O_TEXT) setmode(channel->fd, O_TEXT);
  return oldmode == O_BINARY;
#else
  return 1;
#endif
}

/* Output */

/* Attempt to flush the buffer. This will make room in the buffer for
   at least one character. Returns an error code, or 0 on success.
   *emptied is set to true if the buffer was emptied, or false
   if some data remains in the buffer or an error occurred.
 */

static io_result flush_partial(struct channel *channel, int* emptied)
{
  io_result err;
  int towrite, written;

  towrite = channel->curr - channel->buff;
  CAMLassert (towrite >= 0);
  if (towrite > 0) {
    err = caml_write_fd(channel->fd, channel->flags,
                        channel->buff, towrite,
                        &written);
    if (err) {
      *emptied = 0;
      return err;
    }
    channel->offset += written;
    if (written < towrite)
      memmove(channel->buff, channel->buff + written, towrite - written);
    channel->curr -= written;
  }
  *emptied = (channel->curr == channel->buff);
  return 0;
}

/* Flush completely the buffer. */

CAMLexport void caml_flush(struct channel *channel)
{
  io_result err;
  int emptied;
  Lock(channel);
  do {
    err = flush_partial(channel, &emptied);
  } while (check_retry(channel, err) || !emptied);
  Unlock(channel);
}

/* Output data */

CAMLexport void caml_putch(struct channel* channel, unsigned char ch)
{
  caml_putblock(channel, (char*)&ch, 1);
}

CAMLexport void caml_putword(struct channel *channel, uint32_t w)
{
  unsigned char out[] = {w >> 24, w >> 16, w >> 8, w};
  if (! caml_channel_binary_mode(channel))
    caml_failwith("output_binary_int: not a binary channel");
  caml_really_putblock(channel, (char*)out, 4);
}

CAMLexport io_result caml_try_putblock(struct channel *channel, char *p,
                                       intnat len, int* written_out)
{
  int n, free, towrite, written;

  n = len >= INT_MAX ? INT_MAX : (int) len;
  free = channel->end - channel->curr;
  if (n < free) {
    /* Write request small enough to fit in buffer: transfer to buffer. */
    memmove(channel->curr, p, n);
    channel->curr += n;
    *written_out = n;
    return 0;
  } else {
    /* Write request overflows buffer (or just fills it up): transfer whatever
       fits to buffer and write the buffer */
    io_result err;
    memmove(channel->curr, p, free);
    towrite = channel->end - channel->buff;
    err = caml_write_fd(channel->fd, channel->flags,
                        channel->buff, towrite, &written);
    if (err) {
      *written_out = 0;
      return err;
    }

    if (written < towrite)
      memmove(channel->buff, channel->buff + written, towrite - written);
    channel->offset += written;
    channel->curr = channel->end - written;
    *written_out = free;
    return 0;
  }
}

CAMLexport int caml_putblock(struct channel* channel, char* p, intnat len)
{
  int written;
  io_result err;
  Lock(channel);
  do {
    err = caml_try_putblock(channel, p, len, &written);
  } while (check_retry(channel, err));
  Unlock(channel);
  return written;
}

CAMLexport void caml_really_putblock(struct channel *channel,
                                     char *p, intnat len)
{
  int written;
  Lock(channel);
  while (len > 0) {
    io_result err;
    do {
      err = caml_try_putblock(channel, p, len, &written);
    } while (check_retry(channel, err));
    p += written;
    len -= written;
  }
  Unlock(channel);
}

static io_result caml_try_seek_out(struct channel *channel,
                                       file_offset dest)
{
  io_result err;
  int emptied;
  do {
    err = flush_partial(channel, &emptied);
  } while (!err && !emptied);
  if (err) return err;

  err = caml_lseek_set(channel->fd, dest);
  if (err) return err;

  return 0;
}

CAMLexport file_offset caml_pos_out(struct channel *channel)
{
  return channel->offset + (file_offset)(channel->curr - channel->buff);
}

/* Input */

/* caml_do_read is exported for Cash */
CAMLexport int caml_do_read(int fd, char *p, unsigned int n)
{
  io_result err;
  int res;
  err = caml_read_fd(fd, 0, p, n, &res);
  if (err) raise_error(err);
  return res;
}

CAMLexport unsigned char caml_getch(struct channel* channel)
{
  char ch;
  if (caml_getblock(channel, &ch, 1) != 1)
    caml_raise_end_of_file();
  return (unsigned char)ch;
}

CAMLexport uint32_t caml_getword(struct channel *channel)
{
  int i;
  unsigned char w[4];
  uint32_t res;

  if (! caml_channel_binary_mode(channel))
    caml_failwith("input_binary_int: not a binary channel");
  if (caml_really_getblock(channel, (char*)w, 4) != 4)
    caml_raise_end_of_file();
  res = 0;
  for(i = 0; i < 4; i++) {
    res = (res << 8) + w[i];
  }
  return res;
}

static io_result try_refill(struct channel* channel, intnat len,
                            int* found_len)
{
  int n, avail, nread;

  n = len >= INT_MAX ? INT_MAX : (int) len;
  avail = channel->max - channel->curr;
  if (n <= avail) {
    *found_len = n;
    return 0;
  } else if (avail > 0) {
    *found_len = avail;
    return 0;
  } else {
    io_result err;
    err = caml_read_fd(channel->fd, channel->flags, channel->buff,
                       channel->end - channel->buff,
                       &nread);
    if (err) {
      *found_len = 0;
      return err;
    }
    channel->offset += nread;
    channel->max = channel->buff + nread;
    channel->curr = channel->buff;
    if (n > nread) n = nread;
    *found_len = n;
    return 0;
  }
}

CAMLexport io_result caml_try_getblock(struct channel *channel, char *p,
                                       intnat len, int* n)
{
  io_result err;

  err = try_refill(channel, len, n);
  if (err) return err;

  memmove(p, channel->curr, *n);
  channel->curr += *n;
  return 0;
}

CAMLexport int caml_getblock(struct channel* channel, char* p, intnat len)
{
  io_result err;
  int read;
  Lock(channel);
  do {
    err = caml_try_getblock(channel, p, len, &read);
  } while (check_retry(channel, err));
  Unlock(channel);
  return read;
}

/* Returns the number of bytes read. */
CAMLexport intnat caml_really_getblock(struct channel *chan, char *p, intnat n)
{
  intnat k = n;
  int r;
  Lock (chan);
  while (k > 0) {
    io_result err;
    do {
      err = caml_try_getblock(chan, p, k, &r);
    } while (check_retry(chan, err));
    if (r == 0) break;
    p += r;
    k -= r;
  }
  Unlock (chan);
  return n - k;
}

CAMLexport io_result caml_try_seek_in(struct channel *channel, file_offset dest)
{
  if (dest >= channel->offset - (channel->max - channel->buff) &&
      dest <= channel->offset) {
    channel->curr = channel->max - (channel->offset - dest);
    return 0;
  } else {
    io_result err;
    err = caml_lseek_set(channel->fd, dest);
    if (err) return err;

    channel->offset = dest;
    channel->curr = channel->max = channel->buff;
    return 0;
  }
}

CAMLexport file_offset caml_pos_in(struct channel *channel)
{
  return channel->offset - (file_offset)(channel->max - channel->curr);
}

static io_result caml_try_input_scan_line(struct channel *channel,
                                          intnat* length)
{
  char * p;
  int n;
  io_result err;

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
        *length = -(channel->max - channel->curr);
        return 0;
      }
      /* Fill the buffer as much as possible */
      err = caml_read_fd(channel->fd, channel->flags,
                         channel->max, channel->end - channel->max,
                         &n);
      if (err) {
        *length = 0;
        return err;
      }
      if (n == 0) {
        /* End-of-file encountered. Return the number of characters in the
           buffer, with negative sign since we haven't encountered
           a newline. */
        *length = -(channel->max - channel->curr);
        return 0;
      }
      channel->offset += n;
      channel->max += n;
    }
  } while (*p++ != '\n');
  /* Found a newline. Return the length of the line, newline included. */
  *length = (p - channel->curr);
  return 0;
}

/* OCaml entry points for the I/O functions.  Wrap struct channel *
   objects into a heap-allocated object.  Perform locking
   and unlocking around the I/O operations. */

/* FIXME CAMLexport, but not in io.h  exported for Cash ? */
CAMLexport void caml_finalize_channel(value vchan)
{
  struct channel * chan = Channel(vchan);
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
  custom_compare_ext_default
};

CAMLexport value caml_alloc_channel(struct channel *chan)
{
  value res;
  chan->refcount++;             /* prevent finalization during next alloc */
  res = caml_alloc_custom(&channel_operations, sizeof(struct channel *),
                          1, 1000);
  Channel(res) = chan;
  return res;
}

CAMLprim value caml_ml_open_descriptor_in(value fd)
{
  return caml_alloc_channel(caml_open_descriptor_in(Int_val(fd)));
}

CAMLprim value caml_ml_open_descriptor_out(value fd)
{
  return caml_alloc_channel(caml_open_descriptor_out(Int_val(fd)));
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
     immediate flush_partial or refill, thus raising a Sys_error
     exception */
  // FIXME really?
  channel->curr = channel->max = channel->end;

  if (do_syscall) {
    caml_enter_blocking_section();
    result = CAML_SYS_CLOSE(fd);
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

CAMLprim value caml_ml_channel_size(value vchannel)
{
  file_offset size = caml_channel_size(Channel(vchannel));
  if (size > Max_long) { errno = EOVERFLOW; caml_sys_error(NO_ARG); }
  return Val_long(size);
}

CAMLprim value caml_ml_channel_size_64(value vchannel)
{
  return Val_file_offset(caml_channel_size(Channel(vchannel)));
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
#endif
  return Val_unit;
}

/*
   If the channel is closed, DO NOT raise a "bad file descriptor"
   exception, but do nothing (the buffer is already empty).
   This is because some libraries will flush at exit, even on
   file descriptors that may be closed.
*/

CAMLprim value caml_ml_flush_partial(value vchannel)
{
  CAMLparam1 (vchannel);
  struct channel * channel = Channel(vchannel);
  int res;
  io_result err;

  if (channel->fd == -1) CAMLreturn(Val_true);
  Lock(channel);
  do {
    err = flush_partial(channel, &res);
  } while (check_retry(channel, err));
  Unlock(channel);
  CAMLreturn (Val_bool(res));
}

CAMLprim value caml_ml_flush(value vchannel)
{
  CAMLparam1 (vchannel);
  struct channel * channel = Channel(vchannel);

  if (channel->fd == -1) CAMLreturn(Val_unit);
  caml_flush(channel);
  CAMLreturn (Val_unit);
}

CAMLprim value caml_ml_output_char(value vchannel, value ch)
{
  CAMLparam2 (vchannel, ch);
  caml_putch(Channel(vchannel), Long_val(ch));
  CAMLreturn (Val_unit);
}

CAMLprim value caml_ml_output_int(value vchannel, value w)
{
  CAMLparam2 (vchannel, w);
  caml_putword(Channel(vchannel), Long_val(w));
  CAMLreturn (Val_unit);
}

CAMLprim value caml_ml_output_partial(value vchannel, value buff, value start,
                                      value length)
{
  CAMLparam4 (vchannel, buff, start, length);
  struct channel * channel = Channel(vchannel);
  int res;

  res = caml_putblock(channel, &Byte(buff, Long_val(start)), Long_val(length));
  CAMLreturn (Val_int(res));
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
      io_result err;
      int written;
      do {
        err = caml_try_putblock(channel, &Byte(buff, pos), len, &written);
      } while (check_retry(channel, err));
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
  io_result err;

  Lock(channel);
  do {
    err = caml_try_seek_out(channel, Long_val(pos));
  } while (check_retry(channel, err));
  Unlock(channel);
  CAMLreturn (Val_unit);
}

CAMLprim value caml_ml_seek_out_64(value vchannel, value pos)
{
  CAMLparam2 (vchannel, pos);
  struct channel * channel = Channel(vchannel);
  io_result err;

  Lock(channel);
  do {
    err = caml_try_seek_out(channel, File_offset_val(pos));
  } while (check_retry(channel, err));
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

  c = caml_getch(channel);
  CAMLreturn (Val_long(c));
}

CAMLprim value caml_ml_input_int(value vchannel)
{
  CAMLparam1 (vchannel);
  struct channel * channel = Channel(vchannel);
  intnat i;

  i = caml_getword(channel);
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
  int n;
  io_result err;

  Lock(channel);
  /* We cannot call caml_try_getblock here because buff may move during
     caml_read_fd */
  do {
    err = try_refill(channel, Long_val(vlength), &n);
  } while (check_retry(channel, err));
  memmove(&Byte(buff, Long_val(vstart)), channel->curr, n);
  channel->curr += n;
  Unlock(channel);

  CAMLreturn (Val_long(n));
}

CAMLprim value caml_ml_seek_in(value vchannel, value pos)
{
  CAMLparam2 (vchannel, pos);
  struct channel * channel = Channel(vchannel);
  io_result err;

  Lock(channel);
  do {
    err = caml_try_seek_in(channel, Long_val(pos));
  } while (check_retry(channel, err));
  Unlock(channel);

  CAMLreturn (Val_unit);
}

CAMLprim value caml_ml_seek_in_64(value vchannel, value pos)
{
  CAMLparam2 (vchannel, pos);
  struct channel * channel = Channel(vchannel);
  io_result err;

  Lock(channel);
  do {
    err = caml_try_seek_in(channel, File_offset_val(pos));
  } while (check_retry(channel, err));
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
  io_result err;

  Lock(channel);
  do {
    err = caml_try_input_scan_line(channel, &res);
  } while (check_retry(channel, err));
  Unlock(channel);

  CAMLreturn (Val_long(res));
}
