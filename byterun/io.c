/***********************************************************************/
/*                                                                     */
/*                         Caml Special Light                          */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1995 Institut National de Recherche en Informatique et   */
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

/* Common functions. */

struct channel * open_descr(fd)
     int fd;
{
  struct channel * channel;

  channel = (struct channel *) stat_alloc(sizeof(struct channel));
  channel->fd = fd;
  channel->offset = 0;
  channel->curr = channel->max = channel->buff;
  channel->end = channel->buff + IO_BUFFER_SIZE;
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

value channel_size(channel)      /* ML */
     struct channel * channel;
{
  long end;

  end = lseek(channel->fd, 0, 2);
  if (end == -1) sys_error(NULL);
  if (lseek(channel->fd, channel->offset, 0) != channel->offset) 
    sys_error(NULL);
  return Val_long(end);
}

/* Output */

static void really_write(fd, p, n)
     int fd;
     char * p;
     int n;
{
  int retcode;
  while (n > 0) {
#ifdef HAS_UI
    retcode = ui_write(fd, p, n);
#else
#ifdef EINTR
    do { retcode = write(fd, p, n); } while (retcode == -1 && errno == EINTR);
#else
    retcode = write(fd, p, n);
#endif
#endif
    if (retcode == -1) sys_error(NULL);
    p += retcode;
    n -= retcode;
  }
}   

value flush(channel)            /* ML */
     struct channel * channel;
{
  int n;
  n = channel->max - channel->buff;
  if (n > 0) {
    really_write(channel->fd, channel->buff, n);
    channel->offset += n;
    channel->curr = channel->buff;
    channel->max  = channel->buff;
  }
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

void putblock(channel, p, len)
     struct channel * channel;
     char * p;
     long len;
{
  int n, m;

  n = len >= INT_MAX ? INT_MAX : (int) len;
  m = channel->end - channel->curr;
  if (channel->curr == channel->buff && n >= m) {
    really_write(channel->fd, p, n);
    channel->offset += n;
  } else if (n <= m) {
    bcopy(p, channel->curr, n);
    channel->curr += n;
    if (channel->curr > channel->max) channel->max = channel->curr;
  } else {
    bcopy(p, channel->curr, m);
    p += m;
    n -= m;
    m = channel->end - channel->buff;
    really_write(channel->fd, channel->buff, m);
    channel->offset += m;
    if (n <= m) {
      bcopy(p, channel->buff, n);
      channel->curr = channel->max = channel->buff + n;
    } else {
      really_write(channel->fd, p, n);
      channel->offset += n;
      channel->curr = channel->max = channel->buff;
    }
  }
}

value output(channel, buff, start, length) /* ML */
     value channel, buff, start, length;
{
  putblock((struct channel *) channel,
           &Byte(buff, Long_val(start)),
           Long_val(length));
  return Val_unit;
}

value seek_out(channel, pos)    /* ML */
     struct channel * channel;
     value pos;
{
  long dest;

  dest = Long_val(pos);
  if (dest >= channel->offset &&
      dest <= channel->offset + channel->max - channel->buff) {
    channel->curr = channel->buff + dest - channel->offset;
  } else {
    flush(channel);
    if (lseek(channel->fd, dest, 0) != dest) sys_error(NULL);
    channel->offset = dest;
  }
  return Val_unit;
}

value pos_out(channel)          /* ML */
     struct channel * channel;
{
  return Val_long(channel->offset + channel->curr - channel->buff);
}

value close_out(channel)     /* ML */
     struct channel * channel;
{
  flush(channel);
  close(channel->fd);
  stat_free((char *) channel);
  return Val_unit;
}

/* Input */

static int really_read(fd, p, n)
     int fd;
     char * p;
     unsigned n;
{
  int retcode;

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
  if (retcode == -1) sys_error(NULL);
  return retcode;
}

unsigned char refill(channel)
     struct channel * channel;
{
  int n;

  n = really_read(channel->fd, channel->buff, IO_BUFFER_SIZE);
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
#ifdef SIXTYFOUR
  i = (i << 32) >> 32;          /* Force sign extension */
#endif
  return Val_long(i);
}

int getblock(channel, p, len)
     struct channel * channel;
     char * p;
     long len;
{
  int n, m, l;

  n = len >= INT_MAX ? INT_MAX : (int) len;
  m = channel->max - channel->curr;
  if (n <= m) {
    bcopy(channel->curr, p, n);
    channel->curr += n;
    return n;
  } else if (m > 0) {
    bcopy(channel->curr, p, m);
    channel->curr += m;
    return m;
  } else if (n < IO_BUFFER_SIZE) {
    l = really_read(channel->fd, channel->buff, IO_BUFFER_SIZE);
    channel->offset += l;
    channel->max = channel->buff + l;
    if (n > l) n = l;
    bcopy(channel->buff, p, n);
    channel->curr = channel->buff + n;
    return n;
  } else {
    channel->curr = channel->buff;
    channel->max = channel->buff;
    l = really_read(channel->fd, p, n);
    channel->offset += l;
    return l;
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
    if (lseek(channel->fd, dest, 0) != dest) sys_error(NULL);
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

value close_in(channel)     /* ML */
     struct channel * channel;
{
  close(channel->fd);
  stat_free((char *) channel);
  return Val_unit;
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
      n = really_read(channel->fd, channel->max, channel->end - channel->max);
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
