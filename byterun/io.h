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
  value final_fun;              /* Finalization function */
  int fd;                       /* Unix file descriptor */
  long offset;                  /* Absolute position of fd in the file */
  char * end;                   /* Physical end of the buffer */
  char * curr;                  /* Current position in the buffer */
  char * max;                   /* Logical end of the buffer (for input) */
  char buff[IO_BUFFER_SIZE];    /* The buffer itself */
};

/* For an output channel:
     [offset] is the absolute position of the beginning of the buffer [buff].
   For an input channel:
     [offset] is the absolute position of the logical end of the buffer, [max].
*/

#define putch(channel, ch)                                                  \
  { if ((channel)->curr >= (channel)->end) flush_partial(channel);          \
    *((channel)->curr)++ = (ch); }

#define getch(channel)                                                      \
  ((channel)->curr >= (channel)->max                                        \
   ? refill(channel)                                                        \
   : (unsigned char) *((channel))->curr++)

struct channel * open_descr P((int));
value close_channel P((struct channel *));

value flush_partial P((struct channel *));
value flush P((struct channel *));
void putword P((struct channel *, uint32));
int putblock P((struct channel *, char *, long));
void really_putblock P((struct channel *, char *, long));

unsigned char refill P((struct channel *));
uint32 getword P((struct channel *));
int getblock P((struct channel *, char *, long));
int really_getblock P((struct channel *, char *, long));

#endif /* _io_ */
