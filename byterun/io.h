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

/* Buffered input/output */

#ifndef _io_
#define _io_


#include "misc.h"
#include "mlvalues.h"

#ifndef IO_BUFFER_SIZE
#define IO_BUFFER_SIZE 4096
#endif

struct channel {
  int fd;                       /* Unix file descriptor */
  long offset;                  /* Absolute position of fd in the file */
  char * end;                   /* Physical end of the buffer */
  char * curr;                  /* Current position in the buffer */
  char * max;                   /* Logical end of the buffer (for input) */
  void * mutex;                 /* Placeholder for mutex (for systhreads) */
  char buff[IO_BUFFER_SIZE];    /* The buffer itself */
};

/* For an output channel:
     [offset] is the absolute position of the beginning of the buffer [buff].
   For an input channel:
     [offset] is the absolute position of the logical end of the buffer, [max].
*/

/* Functions and macros that can be called from C.  Take arguments of
   type struct channel *.  No locking is performed. */

#define putch(channel, ch)                                                  \
  { if ((channel)->curr >= (channel)->end) flush_partial(channel);          \
    *((channel)->curr)++ = (ch); }

#define getch(channel)                                                      \
  ((channel)->curr >= (channel)->max                                        \
   ? refill(channel)                                                        \
   : (unsigned char) *((channel))->curr++)

struct channel * open_descriptor (int);
void close_channel (struct channel *);

int flush_partial (struct channel *);
void flush (struct channel *);
void putword (struct channel *, uint32);
int putblock (struct channel *, char *, long);
void really_putblock (struct channel *, char *, long);

unsigned char refill (struct channel *);
uint32 getword (struct channel *);
int getblock (struct channel *, char *, long);
int really_getblock (struct channel *, char *, long);

/* Extract a struct channel * from the heap object representing it */

#define Channel(v) ((struct channel *) Field(v, 1))

/* The locking machinery */

extern void (*channel_mutex_free) (struct channel *);
extern void (*channel_mutex_lock) (struct channel *);
extern void (*channel_mutex_unlock) (struct channel *);
extern void (*channel_mutex_unlock_exn) (void);

#define Lock(channel) \
  if (channel_mutex_lock != NULL) (*channel_mutex_lock)(channel)
#define Unlock(channel) \
  if (channel_mutex_unlock != NULL) (*channel_mutex_unlock)(channel)
#define Unlock_exn() \
  if (channel_mutex_unlock_exn != NULL) (*channel_mutex_unlock_exn)()

#endif /* _io_ */
