/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

/* Buffered input/output */

#ifndef CAML_IO_H
#define CAML_IO_H

#include "misc.h"
#include "mlvalues.h"

#ifndef IO_BUFFER_SIZE
#define IO_BUFFER_SIZE 4096
#endif

#ifdef HAS_OFF_T
#include <sys/types.h>
typedef off_t file_offset;
#else
typedef long file_offset;
#endif

struct channel {
  int fd;                       /* Unix file descriptor */
  file_offset offset;           /* Absolute position of fd in the file */
  char * end;                   /* Physical end of the buffer */
  char * curr;                  /* Current position in the buffer */
  char * max;                   /* Logical end of the buffer (for input) */
  void * mutex;                 /* Placeholder for mutex (for systhreads) */
  struct channel * next;        /* Linear chaining of channels (flush_all) */
  int revealed;                 /* For Cash only */
  int old_revealed;             /* For Cash only */
  int refcount;                 /* For flush_all and for Cash */
  char buff[IO_BUFFER_SIZE];    /* The buffer itself */
};

/* For an output channel:
     [offset] is the absolute position of the beginning of the buffer [buff].
   For an input channel:
     [offset] is the absolute position of the logical end of the buffer, [max].
*/

/* Functions and macros that can be called from C.  Take arguments of
   type struct channel *.  No locking is performed. */

#define putch(channel, ch) do{                                            \
  if ((channel)->curr >= (channel)->end) flush_partial(channel);          \
  *((channel)->curr)++ = (ch);                                            \
}while(0)

#define getch(channel)                                                      \
  ((channel)->curr >= (channel)->max                                        \
   ? refill(channel)                                                        \
   : (unsigned char) *((channel)->curr)++)

CAMLextern struct channel * open_descriptor_in (int);
CAMLextern struct channel * open_descriptor_out (int);
CAMLextern void close_channel (struct channel *);
CAMLextern int channel_binary_mode (struct channel *);

CAMLextern int flush_partial (struct channel *);
CAMLextern void flush (struct channel *);
CAMLextern void putword (struct channel *, uint32);
CAMLextern int putblock (struct channel *, char *, long);
CAMLextern void really_putblock (struct channel *, char *, long);

CAMLextern unsigned char refill (struct channel *);
CAMLextern uint32 getword (struct channel *);
CAMLextern int getblock (struct channel *, char *, long);
CAMLextern int really_getblock (struct channel *, char *, long);

/* Extract a struct channel * from the heap object representing it */

#define Channel(v) (*((struct channel **) (Data_custom_val(v))))

/* The locking machinery */

CAMLextern void (*channel_mutex_free) (struct channel *);
CAMLextern void (*channel_mutex_lock) (struct channel *);
CAMLextern void (*channel_mutex_unlock) (struct channel *);
CAMLextern void (*channel_mutex_unlock_exn) (void);

#define Lock(channel) \
  if (channel_mutex_lock != NULL) (*channel_mutex_lock)(channel)
#define Unlock(channel) \
  if (channel_mutex_unlock != NULL) (*channel_mutex_unlock)(channel)
#define Unlock_exn() \
  if (channel_mutex_unlock_exn != NULL) (*channel_mutex_unlock_exn)()

/* Conversion between file_offset and int64 */

#ifdef ARCH_INT64_TYPE
#define Val_file_offset(fofs) copy_int64(fofs)
#define File_offset_val(v) ((file_offset) Int64_val(v))
#else
CAMLextern value Val_file_offset(file_offset fofs);
CAMLextern file_offset File_offset_val(value v);
#endif

#endif /* CAML_IO_H */
