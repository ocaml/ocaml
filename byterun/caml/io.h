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
#elif defined(HAS_OFF_T)
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
#if defined(NATIVE_CODE) && defined(WITH_SPACETIME)
  CHANNEL_FLAG_BLOCKING_WRITE = 2, /* Don't release master lock when writing */
#endif
  CHANNEL_FLAG_MANAGED_BY_GC = 4,  /* Free and close using GC finalization */
};

/* For an output channel:
     [offset] is the absolute position of the beginning of the buffer [buff].
   For an input channel:
     [offset] is the absolute position of the logical end of the buffer, [max].
*/

/* Functions and macros that can be called from C.  Take arguments of
   type struct channel *.  No locking is performed. */

#define caml_putch(channel, ch) do{                                       \
  if ((channel)->curr >= (channel)->end) caml_flush_partial(channel);     \
  *((channel)->curr)++ = (ch);                                            \
}while(0)

#define caml_getch(channel)                                                 \
  ((channel)->curr >= (channel)->max                                        \
   ? caml_refill(channel)                                                   \
   : (unsigned char) *((channel)->curr)++)

CAMLpublic struct channel * caml_open_descriptor_in (int);
CAMLpublic struct channel * caml_open_descriptor_out (int);
CAMLpublic void caml_close_channel (struct channel *);
CAMLpublic int caml_channel_binary_mode (struct channel *);
CAMLpublic value caml_alloc_channel(struct channel *chan);

CAMLpublic int caml_flush_partial (struct channel *);
CAMLpublic void caml_flush (struct channel *);
CAMLpublic void caml_putword (struct channel *, uint32_t);
CAMLpublic int caml_putblock (struct channel *, char *, intnat);
CAMLpublic void caml_really_putblock (struct channel *, char *, intnat);

CAMLpublic unsigned char caml_refill (struct channel *);
CAMLpublic uint32_t caml_getword (struct channel *);
CAMLpublic int caml_getblock (struct channel *, char *, intnat);
CAMLpublic intnat caml_really_getblock (struct channel *, char *, intnat);

/* Extract a struct channel * from the heap object representing it */

#define Channel(v) (*((struct channel **) (Data_custom_val(v))))

/* The locking machinery */

CAMLdata void (*caml_channel_mutex_free) (struct channel *);
CAMLdata void (*caml_channel_mutex_lock) (struct channel *);
CAMLdata void (*caml_channel_mutex_unlock) (struct channel *);
CAMLdata void (*caml_channel_mutex_unlock_exn) (void);

CAMLdata struct channel * caml_all_opened_channels;

#define Lock(channel) \
  if (caml_channel_mutex_lock != NULL) (*caml_channel_mutex_lock)(channel)
#define Unlock(channel) \
  if (caml_channel_mutex_unlock != NULL) (*caml_channel_mutex_unlock)(channel)
#define Unlock_exn() \
  if (caml_channel_mutex_unlock_exn != NULL) (*caml_channel_mutex_unlock_exn)()

/* Conversion between file_offset and int64_t */

#define Val_file_offset(fofs) caml_copy_int64(fofs)
#define File_offset_val(v) ((file_offset) Int64_val(v))

#endif /* CAML_INTERNALS */

#endif /* CAML_IO_H */
