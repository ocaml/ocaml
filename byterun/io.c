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

/* Buffered input/output. */

#include <errno.h>
#include <fcntl.h>
#include <string.h>
#include "config.h"
#ifdef HAS_UNISTD
#include <unistd.h>
#endif
#ifdef __STDC__
#include <limits.h>
#endif
#include "alloc.h"
#include "fail.h"
#include "io.h"
#include "memory.h"
#include "misc.h"
#include "mlvalues.h"
#include "signals.h"
#include "sys.h"
#ifdef HAS_UI
#include "ui.h"
#endif

#ifndef INT_MAX
#define INT_MAX 0x7FFFFFFF
#endif

#ifndef SEEK_SET
#define SEEK_SET 0
#define SEEK_CUR 1
#define SEEK_END 2
#endif

/* Common functions. */

static void finalize_channel(channel)
     value channel;
{
  struct channel * ch = (struct channel *) channel;
  stat_free(ch->buff);
}

struct channel * open_descr(fd)
     int fd;
{
  char * buffer;
  struct channel * channel;

  buffer = stat_alloc(IO_BUFFER_SIZE);
  channel = (struct channel *)
              alloc_final(sizeof(struct channel) / sizeof(value),
                          finalize_channel,
                          1, 32);
  channel->fd = fd;
  channel->offset = 0;
  channel->buff = buffer;
  channel->end = buffer + IO_BUFFER_SIZE;
  channel->curr = channel->max = buffer;
  return channel;
}

value open_descriptor(fd)       /* ML */
     value fd;
{
  return (value) open_descr(Int_val(fd));
}

value channel_descriptor(channel)   /* ML */
     struct channel * channel;
{
  return Val_long(channel->fd);
}

value close_channel(channel)      /* ML */
     struct channel * channel;
{
  /* For output channels, must have flushed before */
  close(channel->fd);
  channel->fd = -1;
  return Val_unit;
}  

value channel_size(channel)      /* ML */
     struct channel * channel;
{
  long end;

  end = lseek(channel->fd, 0, SEEK_END);
  if (end == -1) sys_error(NO_ARG);
  if (lseek(channel->fd, channel->offset, SEEK_SET) != channel->offset) 
    sys_error(NO_ARG);
  return Val_long(end);
}

/* Output */

#ifndef EINTR
#define EINTR (-1)
#endif
#ifndef EAGAIN
#define EAGAIN (-1)
#endif
#ifndef EWOULDBLOCK
#define EWOULDBLOCK (-1)
#endif

static int do_write(fd, p, n)
     int fd;
     char * p;
     int n;
{
  int retcode;

  Assert(!Is_young(p));
#ifdef HAS_UI
  retcode = ui_write(fd, p, n);
#else
again:
  enter_blocking_section();
  retcode = write(fd, p, n);
  leave_blocking_section();
  if (retcode == -1) {
    if (errno == EINTR) goto again;
    if (errno == EAGAIN || errno == EWOULDBLOCK) {
      /* We couldn't do a partial write here, probably because
	 n <= PIPE_BUF and POSIX says that writes of less than
	 PIPE_BUF characters must be atomic.
	 So, we force a partial write of 1 character.
	 This should always succeed if we've done a select
	 on writing just before. */
      if (n > 1) { n = 1; goto again; }
    }
  }
#endif
  if (retcode == -1) sys_error(NO_ARG);
  return retcode;
}

/* Attempt to flush the buffer. This will make room in the buffer for
   at least one character. Returns true if the buffer is empty at the
   end of the flush, or false if some data remains in the buffer. */

value flush_partial(channel)            /* ML */
     struct channel * channel;
{
  int towrite, written;
  towrite = channel->curr - channel->buff;
  if (towrite > 0) {
    written = do_write(channel->fd, channel->buff, towrite);
    channel->offset += written;
    if (written < towrite)
      bcopy(channel->buff + written, channel->buff, towrite - written);
    channel->curr -= written;
  }
  return Val_bool(channel->curr == channel->buff);
}

/* Flush completely the buffer. */

value flush(channel)            /* ML */
     struct channel * channel;
{
  while (flush_partial(channel) == Val_false) /*nothing*/;
  return Val_unit;
}

value output_char(channel, ch)  /* ML */
     struct channel * channel;
     value ch;
{
  putch(channel, Long_val(ch));
  return Val_unit;
}

void putword(channel, w)
     struct channel * channel;
     uint32 w;
{
  putch(channel, w >> 24);
  putch(channel, w >> 16);
  putch(channel, w >> 8);
  putch(channel, w);
}

value output_int(channel, w)    /* ML */
     struct channel * channel;
     value w;
{
  putword(channel, Long_val(w));
  return Val_unit;
}

int putblock(channel, p, len)
     struct channel * channel;
     char * p;
     long len;
{
  int n, free, towrite, written;

  n = len >= INT_MAX ? INT_MAX : (int) len;
  free = channel->end - channel->curr;
  if (n <= free) {
    /* Write request small enough to fit in buffer: transfer to buffer. */
    bcopy(p, channel->curr, n);
    channel->curr += n;
    return n;
  } else {
    /* Write request overflows buffer: transfer whatever fits to buffer
       and write the buffer */
    bcopy(p, channel->curr, free);
    towrite = channel->end - channel->buff;
    written = do_write(channel->fd, channel->buff, towrite);
    if (written < towrite)
      bcopy(channel->buff + written, channel->buff, towrite - written);
    channel->offset += written;
    channel->curr = channel->end - written;
    channel->max = channel->end - written;
    return free;
  }
}

void really_putblock(channel, p, len)
     struct channel * channel;
     char * p;
     long len;
{
  int written;
  while (len > 0) {
    written = putblock(channel, p, len);
    p += written;
    len -= written;
  }
}

value output_partial(channel, buff, start, length) /* ML */
     value channel, buff, start, length;
{
  return Val_int(putblock((struct channel *) channel,
                          &Byte(buff, Long_val(start)),
                          Long_val(length)));
}

value output(channel, buff, start, length) /* ML */
     value channel, buff, start, length;
{
  long pos = Long_val(start);
  long len = Long_val(length);
  Push_roots(r, 1);
  r[0] = buff;
  while (len > 0) {
    int written = putblock((struct channel *) channel, &Byte(r[0], pos), len);
    pos += written;
    len -= written;
  }
  Pop_roots();
  return Val_unit;
}

value seek_out(channel, pos)    /* ML */
     struct channel * channel;
     value pos;
{
  long dest;
  dest = Long_val(pos);
  flush(channel);
  if (lseek(channel->fd, dest, 0) != dest) sys_error(NO_ARG);
  channel->offset = dest;
  return Val_unit;
}

value pos_out(channel)          /* ML */
     struct channel * channel;
{
  return Val_long(channel->offset + channel->curr - channel->buff);
}

/* Input */

static int do_read(fd, p, n)
     int fd;
     char * p;
     unsigned n;
{
  int retcode;

  Assert(!Is_young(p));
  enter_blocking_section();
#ifdef HAS_UI
  retcode = ui_read(fd, p, n);
#else
#ifdef EINTR
  do { retcode = read(fd, p, n); } while (retcode == -1 && errno == EINTR);
#else
  retcode = read(fd, p, n);
#endif
#endif
  leave_blocking_section();
  if (retcode == -1) sys_error(NO_ARG);
  return retcode;
}

unsigned char refill(channel)
     struct channel * channel;
{
  int n;

  n = do_read(channel->fd, channel->buff, IO_BUFFER_SIZE);
  if (n == 0) raise_end_of_file();
  channel->offset += n;
  channel->max = channel->buff + n;
  channel->curr = channel->buff + 1;
  return (unsigned char)(channel->buff[0]);
}

value input_char(channel)       /* ML */
     struct channel * channel;
{
  unsigned char c;
  c = getch(channel);
  return Val_long(c);
}

uint32 getword(channel)
     struct channel * channel;
{
  int i;
  uint32 res;

  res = 0;
  for(i = 0; i < 4; i++) {
    res = (res << 8) + getch(channel);
  }
  return res;
}

value input_int(channel)        /* ML */
     struct channel * channel;
{
  long i;
  i = getword(channel);
#ifdef ARCH_SIXTYFOUR
  i = (i << 32) >> 32;          /* Force sign extension */
#endif
  return Val_long(i);
}

int getblock(channel, p, len)
     struct channel * channel;
     char * p;
     long len;
{
  int n, avail, nread;

  n = len >= INT_MAX ? INT_MAX : (int) len;
  avail = channel->max - channel->curr;
  if (n <= avail) {
    bcopy(channel->curr, p, n);
    channel->curr += n;
    return n;
  } else if (avail > 0) {
    bcopy(channel->curr, p, avail);
    channel->curr += avail;
    return avail;
  } else if (n < IO_BUFFER_SIZE) {
    nread = do_read(channel->fd, channel->buff, IO_BUFFER_SIZE);
    channel->offset += nread;
    channel->max = channel->buff + nread;
    if (n > nread) n = nread;
    bcopy(channel->buff, p, n);
    channel->curr = channel->buff + n;
    return n;
  } else {
    nread = do_read(channel->fd, p, n);
    channel->offset += nread;
    return nread;
  }
}

int really_getblock(chan, p, n)
     struct channel * chan;
     char * p;
     long n;
{
  int r;
  while (n > 0) {
    r = getblock(chan, p, n);
    if (r == 0) return 0;
    p += r;
    n -= r;
  }
  return 1;
}

value input(channel, buff, start, length) /* ML */
     value channel, buff, start, length;
{
  return Val_long(getblock((struct channel *) channel,
                           &Byte(buff, Long_val(start)),
                           Long_val(length)));
}

value seek_in(channel, pos)     /* ML */
     struct channel * channel;
     value pos;
{
  long dest;

  dest = Long_val(pos);
  if (dest >= channel->offset - (channel->max - channel->buff) &&
      dest <= channel->offset) {
    channel->curr = channel->max - (channel->offset - dest);
  } else {
    if (lseek(channel->fd, dest, SEEK_SET) != dest) sys_error(NO_ARG);
    channel->offset = dest;
    channel->curr = channel->max = channel->buff;
  }
  return Val_unit;
}

value pos_in(channel)           /* ML */
     struct channel * channel;
{
  return Val_long(channel->offset - (channel->max - channel->curr));
}

value input_scan_line(channel)       /* ML */
     struct channel * channel;
{
  char * p;
  int n;

  p = channel->curr;
  do {
    if (p >= channel->max) {
      /* No more characters available in the buffer */
      if (channel->curr > channel->buff) {
        /* Try to make some room in the buffer by shifting the unread
           portion at the beginning */
        bcopy(channel->curr, channel->buff, channel->max - channel->curr);
        n = channel->curr - channel->buff;
        channel->curr -= n;
        channel->max -= n;
        p -= n;
      }
      if (channel->max >= channel->end) {
        /* Buffer is full, no room to read more characters from the input.
           Return the number of characters in the buffer, with negative
           sign to indicate that no newline was encountered. */
        return Val_long(-(channel->max - channel->curr));
      }
      /* Fill the buffer as much as possible */
      n = do_read(channel->fd, channel->max, channel->end - channel->max);
      if (n == 0) {
        /* End-of-file encountered. Return the number of characters in the
           buffer, with negative sign since we haven't encountered 
           a newline. */
        return Val_long(-(channel->max - channel->curr));
      }
      channel->offset += n;
      channel->max += n;
    }
  } while (*p++ != '\n');
  /* Found a newline. Return the length of the line, newline included. */
  return Val_long(p - channel->curr);
}
