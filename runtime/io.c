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
#include "caml/camlatomic.h"
#include "caml/custom.h"
#include "caml/fail.h"
#include "caml/io.h"
#include "caml/memory.h"
#include "caml/misc.h"
#include "caml/mlvalues.h"
#include "caml/osdeps.h"
#include "caml/platform.h"
#include "caml/signals.h"
#include "caml/sys.h"
#include "caml/bigarray.h"

#ifndef SEEK_SET
#define SEEK_SET 0
#define SEEK_CUR 1
#define SEEK_END 2
#endif

#if defined(_WIN32)
#include <io.h>
#define lseek _lseeki64
#endif

/* Representation of channel status and direction:

   Open channels have fd >= 0 && buff != dummy_buff
     Open input channels have max != NULL
     Open output channels have max == NULL

   Closed channels have fd == -1 && buff == dummy_buff && end == dummy_buff+1
     Closed input channels have curr == max == buff (empty buffer)
     Closed output channels have curr == end (full buffer)
       and max == end (not NULL)

*/
static char dummy_buff[1];

/* Locking channels.

   All operations on channels first take the channel lock.

   Exceptions could abort an operation on a channel with its lock
   still held, leaving the channel in an inconsistent state.

   To avoid this, we store the channel currently being locked
   (or NULL) in (thread-local) global state. This can be used to
   define a [caml_channel_cleanup_on_raise] function that unlocks the
   currently locked channel (if any), which is then called by
   [caml_raise].
 */
static CAMLthread_local struct channel* last_channel_locked = NULL;

CAMLexport void caml_channel_lock(struct channel *chan)
{
  if( caml_plat_try_lock(&chan->mutex) ) {
    last_channel_locked = chan;
    return;
  }

  /* If unsuccessful, block on mutex */
  caml_enter_blocking_section();
  caml_plat_lock(&chan->mutex);
  last_channel_locked = chan;
  caml_leave_blocking_section();
}

CAMLexport void caml_channel_unlock(struct channel *chan)
{
  caml_plat_unlock(&chan->mutex);
  last_channel_locked = NULL;
}

CAMLexport void caml_channel_cleanup_on_raise(void)
{
  struct channel * chan = last_channel_locked;
  if (chan != NULL) caml_channel_unlock(chan);
}

/* List of channels opened from the OCaml side and managed by the GC */
CAMLexport struct channel * caml_all_opened_channels = NULL;

/* The mutex protecting the list above */
CAMLexport caml_plat_mutex
  caml_all_opened_channels_mutex = CAML_PLAT_MUTEX_INITIALIZER;

/* Basic functions over type struct channel *.
   These functions can be called directly from C.
   No locking is performed. */

/* Functions shared between input and output */

static void check_pending(struct channel *channel)
{
  if (caml_check_pending_actions()) {
    /* Temporarily unlock the channel, to ensure locks are not held
       while any signal handlers (or finalisers, etc) are running.
       Don't do this for channels allocated and used from C,
       as their locks may or may not be taken depending on the
       usage pattern in the C code. */
    if (channel->flags & CHANNEL_FLAG_MANAGED_BY_GC)
      caml_channel_unlock(channel);
    caml_process_pending_actions();
    if (channel->flags & CHANNEL_FLAG_MANAGED_BY_GC)
      caml_channel_lock(channel);
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

static void link_channel (struct channel* channel)
{
  channel->next = caml_all_opened_channels;
  CAMLassert(channel->prev == NULL);
  if (caml_all_opened_channels != NULL)
    caml_all_opened_channels->prev = channel;
  caml_all_opened_channels = channel;
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
  channel->next = NULL;
  channel->prev = NULL;
}

CAMLexport struct channel * caml_open_descriptor_in(int fd)
{
  struct channel * channel;

  channel = (struct channel *) caml_stat_alloc(sizeof(struct channel));
  channel->buff = (char *) caml_stat_alloc_noexc(IO_BUFFER_SIZE);
  if (channel->buff == NULL){
    caml_stat_free(channel);
    caml_raise_out_of_memory();
  }
  channel->fd = fd;
  caml_enter_blocking_section_no_pending();
  channel->offset = lseek(fd, 0, SEEK_CUR);
  caml_leave_blocking_section();
  channel->curr = channel->max = channel->buff;
  channel->end = channel->buff + IO_BUFFER_SIZE;
  caml_plat_mutex_init(&channel->mutex);
  channel->refcount = 0;
  channel->prev = NULL;
  channel->next = NULL;
  channel->name = NULL;
  channel->flags = descriptor_is_in_binary_mode(fd) ? 0 : CHANNEL_TEXT_MODE;
  return channel;
}

CAMLexport struct channel * caml_open_descriptor_out(int fd)
{
  struct channel * channel;

  channel = caml_open_descriptor_in(fd);
  channel->max = NULL;
  return channel;
}

CAMLexport void caml_close_channel(struct channel *channel)
{
  CAMLassert((channel->flags & CHANNEL_FLAG_MANAGED_BY_GC) == 0);
  close(channel->fd);
  caml_plat_mutex_free(&channel->mutex);
  caml_stat_free(channel->name);
  caml_stat_free(channel->buff);
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
    if (written == -1) {
      if (errno == EINTR) goto again;
      if (errno == EBADF || errno == EPIPE || errno == ECONNRESET) {
        /* This is a permanent failure: retrying the flush later will
           not make it go away. If the channel is not closed, discard
           the buffered data, so that a subsequent close will succeed,
           or the finalizer can reclaim the channel. */
        if (channel->fd != -1) channel->curr = channel->buff;
      }
      caml_sys_io_error(NO_ARG);
    }
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

CAMLexport void caml_flush_if_unbuffered(struct channel *channel)
{
  if (channel->flags & CHANNEL_FLAG_UNBUFFERED) caml_flush(channel);
}

/* Output data */

#define Putch(channel, ch) do{                                            \
  if ((channel)->curr >= (channel)->end) caml_flush_partial(channel);     \
  *((channel)->curr)++ = (ch);                                            \
}while(0)

CAMLexport void caml_putch(struct channel *channel, int ch)
{
  Putch(channel, ch);
}

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
  file_offset res;
  caml_flush(channel);
  caml_enter_blocking_section_no_pending();
  res = lseek(channel->fd, dest, SEEK_SET);
  if (res < 0 || res != dest) {
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
  } while (r == -1 && errno == EINTR);
  return r;
}

CAMLexport unsigned char caml_refill(struct channel *channel)
{
  int n;
 again:
  check_pending(channel);
  n = caml_read_fd(channel->fd, channel->flags,
                   channel->buff, channel->end - channel->buff);
  if (n == -1) {
    if (errno == EINTR) goto again; else caml_sys_io_error(NO_ARG);
  } else if (n == 0) {
    caml_raise_end_of_file();
  }
  channel->offset += n;
  channel->max = channel->buff + n;
  channel->curr = channel->buff + 1;
  return (unsigned char)(channel->buff[0]);
}

#define Getch(channel)                                                      \
  ((channel)->curr >= (channel)->max                                        \
   ? caml_refill(channel)                                                   \
   : (unsigned char) *((channel)->curr)++)

CAMLexport unsigned char caml_getch(struct channel *channel)
{
  return Getch(channel);
}

CAMLexport uint32_t caml_getword(struct channel *channel)
{
  int i;
  uint32_t res;

  if (! caml_channel_binary_mode(channel))
    caml_failwith("input_binary_int: not a binary channel");
  res = 0;
  for(i = 0; i < 4; i++) {
    res = (res << 8) + Getch(channel);
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
    if (nread == -1) {
      if (errno == EINTR) goto again; else caml_sys_io_error(NO_ARG);
    }
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
  file_offset res;
  if (dest >= channel->offset - (channel->max - channel->buff)
      && dest <= channel->offset
      && (channel->flags & CHANNEL_TEXT_MODE) == 0) {
    channel->curr = channel->max - (channel->offset - dest);
  } else {
    caml_enter_blocking_section_no_pending();
    res = lseek(channel->fd, dest, SEEK_SET);
    if (res < 0 || res != dest) {
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
      if (n == -1) {
        if (errno == EINTR) goto again; else caml_sys_io_error(NO_ARG);
      }
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
  int notflushed = 0;
  if ((chan->flags & CHANNEL_FLAG_MANAGED_BY_GC) == 0) return;
  /* Check for channels that have not been closed explicitly. */
  if (chan->fd != -1 && chan->name && caml_runtime_warnings_active())
    fprintf(stderr,
            "[ocaml] channel opened on file '%s' dies without being closed\n",
            chan->name);
  if (chan->max == NULL && chan->curr != chan->buff) {
    /* This is an unclosed out channel (chan->max == NULL) with a
       non-empty buffer: keep it around so the OCaml [at_exit] function
       gets a chance to flush it.  We would want to simply flush the
       channel now, but (i) flushing can raise exceptions, and (ii) it
       is potentially a blocking operation.  Both are forbidden in a
       finalization function.
       Refs: https://github.com/ocaml/ocaml/issues/6902
             https://github.com/ocaml/ocaml/pull/210
    */
    if (chan->name && caml_runtime_warnings_active())
      fprintf(stderr,
              "[ocaml] (moreover, it has unflushed data)\n");
    notflushed = 1;
  }
  /* Don't run concurrently with caml_ml_out_channels_list that may resurrect
     a dead channel . */
  caml_plat_lock (&caml_all_opened_channels_mutex);
  chan->refcount --;
  if (chan->refcount > 0 || notflushed) {
    /* We need to keep the channel around, either because it is being
       added to the list returned by caml_ml_out_channels_list,
       or because it contains unflushed data. */
    caml_plat_unlock (&caml_all_opened_channels_mutex);
    return;
  }
  unlink_channel(chan);
  caml_plat_unlock (&caml_all_opened_channels_mutex);
  caml_plat_mutex_free(&chan->mutex);
  caml_stat_free(chan->name);
  if (chan->fd != -1) caml_stat_free(chan->buff);
  caml_stat_free(chan);
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
  res = caml_alloc_custom(&channel_operations, sizeof(struct channel *), 0, 1);
  Channel(res) = chan;
  return res;
}

CAMLprim value caml_ml_open_descriptor_in_with_flags(int fd, int flags)
{
  struct channel * chan = caml_open_descriptor_in(fd);
  chan->flags |= flags | CHANNEL_FLAG_MANAGED_BY_GC;
  chan->refcount = 1;
  caml_plat_lock (&caml_all_opened_channels_mutex);
  link_channel (chan);
  caml_plat_unlock (&caml_all_opened_channels_mutex);
  return caml_alloc_channel(chan);
}

CAMLprim value caml_ml_open_descriptor_in(value fd) {
  return caml_ml_open_descriptor_in_with_flags(Int_val(fd), 0);
}

CAMLprim value caml_ml_open_descriptor_out_with_flags(int fd, int flags)
{
  struct channel * chan = caml_open_descriptor_out(fd);
  chan->flags |= flags | CHANNEL_FLAG_MANAGED_BY_GC;
  chan->refcount = 1;
  caml_plat_lock (&caml_all_opened_channels_mutex);
  link_channel (chan);
  caml_plat_unlock (&caml_all_opened_channels_mutex);
  return caml_alloc_channel(chan);
}

CAMLprim value caml_ml_open_descriptor_out(value fd) {
  return caml_ml_open_descriptor_out_with_flags(Int_val(fd), 0);
}

CAMLprim value caml_ml_set_channel_name(value vchannel, value vname)
{
  CAMLparam2(vchannel, vname);
  struct channel * channel = Channel(vchannel);
  caml_channel_lock(channel);
  caml_stat_free(channel->name);
  if (caml_string_length(vname) > 0)
    channel->name = caml_stat_strdup(String_val(vname));
  else
    channel->name = NULL;
  caml_channel_unlock(channel);
  CAMLreturn (Val_unit);
}

struct channel_list {
  struct channel* channel;
  struct channel_list* next;
};

CAMLprim value caml_ml_out_channels_list (value unit)
{
  CAMLparam0 ();
  CAMLlocal3 (res, tail, chan);
  struct channel * channel;
  struct channel_list *channel_list = NULL, *cl_tmp;
  mlsize_t i, num_channels = 0;

  caml_plat_lock (&caml_all_opened_channels_mutex);
  for (channel = caml_all_opened_channels;
       channel != NULL;
       channel = channel->next) {
    CAMLassert(channel->flags & CHANNEL_FLAG_MANAGED_BY_GC);
    /* Unclosed output channels are exactly the ones with max == NULL */
    if (channel->max == NULL) {
      /* refcount is incremented here to keep the channel alive */
      channel->refcount ++;
      num_channels++;
      cl_tmp = caml_stat_alloc_noexc (sizeof(struct channel_list));
      if (cl_tmp == NULL)
        caml_fatal_error ("caml_ml_out_channels_list: out of memory");
      cl_tmp->channel = channel;
      cl_tmp->next = channel_list;
      channel_list = cl_tmp;
    }
  }
  caml_plat_unlock (&caml_all_opened_channels_mutex);

  res = Val_emptylist;
  cl_tmp = NULL;
  for (i = 0; i < num_channels; i++) {
    chan = caml_alloc_channel (channel_list->channel);
    tail = res;
    res = caml_alloc_2(Tag_cons, chan, tail);
    cl_tmp = channel_list;
    channel_list = channel_list->next;
    caml_stat_free (cl_tmp);
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
  CAMLparam1(vchannel);
  int result;
  int fd;

  /* For output channels, must have flushed before */
  struct channel * channel = Channel(vchannel);

  caml_channel_lock(channel);
  /* If already closed, we are done */
  if (channel->fd != -1) {
    fd = channel->fd;
    channel->fd = -1;
    caml_stat_free(channel->buff);
    channel->buff = dummy_buff;
    channel->end = dummy_buff + 1;
    /* Make sure that every read or write on the channel will cause an
       immediate caml_flush_partial or caml_refill, thus raising a
       Sys_error exception, and that a non-zero seek cannot stay
       within the buffer.
    */
    if (channel->max == NULL){
      /* closed output channel: full buffer with max != NULL */
      channel->curr = channel->max = channel->end;
    }else{
      /* closed input channel: empty buffer */
      channel->curr = channel->max = channel->buff;
    }
    caml_enter_blocking_section_no_pending();
    result = close(fd);
    caml_leave_blocking_section();

    if (result == -1) caml_sys_error (NO_ARG);
  }
  caml_channel_unlock(channel);
  CAMLreturn (Val_unit);
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

  caml_channel_lock(channel);
  size = caml_channel_size(Channel(vchannel));
  caml_channel_unlock(channel);
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
  CAMLparam2(vchannel, mode);
#if defined(_WIN32) || defined(__CYGWIN__)
  struct channel * channel = Channel(vchannel);
  caml_channel_lock(channel);
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
  caml_channel_unlock(channel);
#endif
  CAMLreturn (Val_unit);
}

CAMLprim value caml_ml_is_binary_mode(value vchannel)
{
  return Val_bool(caml_channel_binary_mode(Channel(vchannel)));
}

/*
   If the channel is closed, DO NOT raise a "bad file descriptor"
   exception, but do nothing.
   This is because some libraries will flush at exit, even on
   channels that may be closed.
*/

CAMLprim value caml_ml_flush(value vchannel)
{
  CAMLparam1 (vchannel);
  struct channel * channel = Channel(vchannel);

  caml_channel_lock(channel);
  if (channel->fd != -1)
    caml_flush(channel);
  caml_channel_unlock(channel);
  CAMLreturn (Val_unit);
}

CAMLprim value caml_ml_set_buffered(value vchannel, value mode)
{
  CAMLparam2(vchannel, mode);
  struct channel * channel = Channel(vchannel);
  caml_channel_lock(channel);
  if (Bool_val(mode)) {
    channel->flags &= ~CHANNEL_FLAG_UNBUFFERED;
  } else {
    channel->flags |= CHANNEL_FLAG_UNBUFFERED;
    if (channel->fd != -1)
      caml_flush(channel);
  }
  caml_channel_unlock(channel);
  CAMLreturn(Val_unit);
}

CAMLprim value caml_ml_is_buffered(value vchannel)
{
  struct channel * channel = Channel(vchannel);
  return Val_bool( ! (channel->flags & CHANNEL_FLAG_UNBUFFERED));
}

CAMLprim value caml_ml_output_char(value vchannel, value ch)
{
  CAMLparam2 (vchannel, ch);
  struct channel * channel = Channel(vchannel);

  caml_channel_lock(channel);
  Putch(channel, Long_val(ch));
  caml_flush_if_unbuffered(channel);
  caml_channel_unlock(channel);
  CAMLreturn (Val_unit);
}

CAMLprim value caml_ml_output_int(value vchannel, value w)
{
  CAMLparam2 (vchannel, w);
  struct channel * channel = Channel(vchannel);

  caml_channel_lock(channel);
  caml_putword(channel, (uint32_t) Long_val(w));
  caml_flush_if_unbuffered(channel);
  caml_channel_unlock(channel);
  CAMLreturn (Val_unit);
}

CAMLprim value caml_ml_output_bytes(value vchannel, value buff, value start,
                              value length)
{
  CAMLparam4 (vchannel, buff, start, length);
  struct channel * channel = Channel(vchannel);
  intnat pos = Long_val(start);
  intnat len = Long_val(length);

  caml_channel_lock(channel);
    /* We cannot call caml_really_putblock here because buff may move
       during caml_write_fd */
    while (len > 0) {
      int written = caml_putblock(channel, &Byte(buff, pos), len);
      pos += written;
      len -= written;
    }
    caml_flush_if_unbuffered(channel);
  caml_channel_unlock(channel);
  CAMLreturn (Val_unit);
}

CAMLprim value caml_ml_output(value vchannel, value buff, value start,
                              value length)
{
  return caml_ml_output_bytes (vchannel, buff, start, length);
}

CAMLprim value caml_ml_output_bigarray(value vchannel, value vbuf,
                                       value vpos, value vlen)
{
  CAMLparam4(vchannel, vbuf, vpos, vlen);
  struct channel * channel = Channel(vchannel);
  intnat pos = Long_val(vpos);
  intnat len = Long_val(vlen);

  caml_channel_lock(channel);
  caml_really_putblock(channel, (char *)Caml_ba_data_val(vbuf) + pos, len);
  caml_channel_unlock(channel);

  CAMLreturn (Val_unit);
}

CAMLprim value caml_ml_seek_out(value vchannel, value pos)
{
  CAMLparam2 (vchannel, pos);
  struct channel * channel = Channel(vchannel);

  caml_channel_lock(channel);
  caml_seek_out(channel, Long_val(pos));
  caml_channel_unlock(channel);
  CAMLreturn (Val_unit);
}

CAMLprim value caml_ml_seek_out_64(value vchannel, value pos)
{
  CAMLparam2 (vchannel, pos);
  struct channel * channel = Channel(vchannel);

  caml_channel_lock(channel);
  caml_seek_out(channel, File_offset_val(pos));
  caml_channel_unlock(channel);
  CAMLreturn (Val_unit);
}

CAMLprim value caml_ml_pos_out(value vchannel)
{
  CAMLparam1 (vchannel);
  file_offset pos;
  struct channel *channel = Channel(vchannel);
  caml_channel_lock(channel);
  pos = caml_pos_out(channel);
  caml_channel_unlock(channel);
  if (pos > Max_long) { errno = EOVERFLOW; caml_sys_error(NO_ARG); }
  CAMLreturn (Val_long(pos));
}

CAMLprim value caml_ml_pos_out_64(value vchannel)
{
  CAMLparam1 (vchannel);
  file_offset pos;
  struct channel *channel = Channel(vchannel);
  caml_channel_lock(channel);
  pos = caml_pos_out(channel);
  caml_channel_unlock(channel);
  CAMLreturn (Val_file_offset(pos));
}

CAMLprim value caml_ml_input_char(value vchannel)
{
  CAMLparam1 (vchannel);
  struct channel * channel = Channel(vchannel);
  unsigned char c;

  caml_channel_lock(channel);
  c = Getch(channel);
  caml_channel_unlock(channel);
  CAMLreturn (Val_long(c));
}

CAMLprim value caml_ml_input_int(value vchannel)
{
  CAMLparam1 (vchannel);
  struct channel * channel = Channel(vchannel);
  intnat i;

  caml_channel_lock(channel);
  i = caml_getword(channel);
  caml_channel_unlock(channel);
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

  caml_channel_lock(channel);
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
    if (nread == -1) {
      if (errno == EINTR) goto again; else caml_sys_io_error(NO_ARG);
    }
    channel->offset += nread;
    channel->max = channel->buff + nread;
    if (n > nread) n = nread;
    memmove(&Byte(buff, start), channel->buff, n);
    channel->curr = channel->buff + n;
  }
  caml_channel_unlock(channel);
  CAMLreturn (Val_long(n));
}

CAMLprim value caml_ml_input_bigarray(value vchannel, value vbuf,
                                      value vpos, value vlen)
{
  CAMLparam4(vchannel, vbuf, vpos, vlen);
  struct channel * channel = Channel(vchannel);
  intnat pos = Long_val(vpos);
  intnat len = Long_val(vlen);
  intnat n;

  caml_channel_lock(channel);
  n = caml_getblock(channel, (char *)Caml_ba_data_val(vbuf) + pos, len);
  caml_channel_unlock(channel);

  CAMLreturn (Val_long(n));
}

CAMLprim value caml_ml_seek_in(value vchannel, value pos)
{
  CAMLparam2 (vchannel, pos);
  struct channel * channel = Channel(vchannel);

  caml_channel_lock(channel);
  caml_seek_in(channel, Long_val(pos));
  caml_channel_unlock(channel);
  CAMLreturn (Val_unit);
}

CAMLprim value caml_ml_seek_in_64(value vchannel, value pos)
{
  CAMLparam2 (vchannel, pos);
  struct channel * channel = Channel(vchannel);

  caml_channel_lock(channel);
  caml_seek_in(channel, File_offset_val(pos));
  caml_channel_unlock(channel);
  CAMLreturn (Val_unit);
}

CAMLprim value caml_ml_pos_in(value vchannel)
{
  CAMLparam1 (vchannel);
  file_offset pos;
  struct channel *channel = Channel(vchannel);
  caml_channel_lock(channel);
  pos = caml_pos_in(channel);
  caml_channel_unlock(channel);
  if (pos > Max_long) { errno = EOVERFLOW; caml_sys_error(NO_ARG); }
  CAMLreturn (Val_long(pos));
}

CAMLprim value caml_ml_pos_in_64(value vchannel)
{
  CAMLparam1 (vchannel);
  file_offset pos;
  struct channel *channel = Channel(vchannel);
  caml_channel_lock(channel);
  pos = caml_pos_in(channel);
  caml_channel_unlock(channel);
  CAMLreturn (Val_file_offset(pos));
}

CAMLprim value caml_ml_input_scan_line(value vchannel)
{
  CAMLparam1 (vchannel);
  struct channel * channel = Channel(vchannel);
  intnat res;

  caml_channel_lock(channel);
  res = caml_input_scan_line(channel);
  caml_channel_unlock(channel);
  CAMLreturn (Val_long(res));
}

CAMLprim value caml_terminfo_rows(value vchannel)
{
  return Val_int(caml_num_rows_fd(Channel(vchannel)->fd));
}
