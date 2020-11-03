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

/* Buffered input/output */

#ifndef CAML_IO_H
#define CAML_IO_H

#ifdef CAML_INTERNALS

#include "misc.h"
#include "mlvalues.h"

#ifndef IO_BUFFER_SIZE
#define IO_BUFFER_SIZE 65536
#endif

#if defined(_WIN32)
typedef __int64 file_offset;
#else
#include <sys/types.h>
typedef off_t file_offset;
#endif

struct channel {
  int fd;                       /* Unix file descriptor */
  file_offset offset;           /* Absolute position of fd in the file */
  char * end;                   /* Physical end of the buffer */
  char * curr;                  /* Current position in the buffer */
  char * max;                   /* Logical end of the buffer (for input) */
  void * mutex;                 /* Placeholder for mutex (for systhreads) */
  struct channel * next, * prev;/* Double chaining of channels (flush_all) */
  int revealed;                 /* For Cash only */
  int old_revealed;             /* For Cash only */
  int refcount;                 /* For flush_all and for Cash */
  int flags;                    /* Bitfield */
  char buff[IO_BUFFER_SIZE];    /* The buffer itself */
  char * name;                  /* Optional name (to report fd leaks) */
};

enum {
  CHANNEL_FLAG_FROM_SOCKET = 1,  /* For Windows */
  CHANNEL_FLAG_MANAGED_BY_GC = 4,  /* Free and close using GC finalization */
  CHANNEL_TEXT_MODE = 8,           /* "Text mode" for Windows and Cygwin */
};

/* For an output channel:
     [offset] is the absolute position of the beginning of the buffer [buff].
   For an input channel:
     [offset] is the absolute position of the logical end of the buffer, [max].
*/

/* Creating and closing channels from C */

CAMLextern struct channel * caml_open_descriptor_in (int);
CAMLextern struct channel * caml_open_descriptor_out (int);
CAMLextern void caml_close_channel (struct channel *);
CAMLextern file_offset caml_channel_size (struct channel *);
CAMLextern void caml_seek_in (struct channel *, file_offset);
CAMLextern void caml_seek_out (struct channel *, file_offset);
CAMLextern file_offset caml_pos_in (struct channel *);
CAMLextern file_offset caml_pos_out (struct channel *);

/* I/O on channels from C. The channel must be locked (see below) before
   calling any of the functions and macros below */

#define caml_putch(channel, ch) do{                                       \
  if ((channel)->curr >= (channel)->end) caml_flush_partial(channel);     \
  *((channel)->curr)++ = (ch);                                            \
}while(0)

#define caml_getch(channel)                                                 \
  ((channel)->curr >= (channel)->max                                        \
   ? caml_refill(channel)                                                   \
   : (unsigned char) *((channel)->curr)++)

CAMLextern value caml_alloc_channel(struct channel *chan);
CAMLextern int caml_channel_binary_mode (struct channel *);

CAMLextern int caml_flush_partial (struct channel *);
CAMLextern void caml_flush (struct channel *);
CAMLextern void caml_putword (struct channel *, uint32_t);
CAMLextern int caml_putblock (struct channel *, char *, intnat);
CAMLextern void caml_really_putblock (struct channel *, char *, intnat);

CAMLextern unsigned char caml_refill (struct channel *);
CAMLextern uint32_t caml_getword (struct channel *);
CAMLextern int caml_getblock (struct channel *, char *, intnat);
CAMLextern intnat caml_really_getblock (struct channel *, char *, intnat);

/* Extract a struct channel * from the heap object representing it */

#define Channel(v) (*((struct channel **) (Data_custom_val(v))))

/* The locking machinery */

CAMLextern void (*caml_channel_mutex_free) (struct channel *);
CAMLextern void (*caml_channel_mutex_lock) (struct channel *);
CAMLextern void (*caml_channel_mutex_unlock) (struct channel *);
CAMLextern void (*caml_channel_mutex_unlock_exn) (void);

CAMLextern struct channel * caml_all_opened_channels;

#define Lock(channel) \
  if (caml_channel_mutex_lock != NULL) (*caml_channel_mutex_lock)(channel)
#define Unlock(channel) \
  if (caml_channel_mutex_unlock != NULL) (*caml_channel_mutex_unlock)(channel)
#define Unlock_exn() \
  if (caml_channel_mutex_unlock_exn != NULL) (*caml_channel_mutex_unlock_exn)()

/* Conversion between file_offset and int64_t */

#define Val_file_offset(fofs) caml_copy_int64(fofs)
#define File_offset_val(v) ((file_offset) Int64_val(v))

/* Primitives required by the Unix library */
CAMLextern value caml_ml_open_descriptor_in(value fd);
CAMLextern value caml_ml_open_descriptor_out(value fd);

#endif /* CAML_INTERNALS */

#endif /* CAML_IO_H */
