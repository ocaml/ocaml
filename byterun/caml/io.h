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
};

/* For an output channel:
     [offset] is the absolute position of the beginning of the buffer [buff].
   For an input channel:
     [offset] is the absolute position of the logical end of the buffer, [max].
*/

/* Functions and macros that can be called from C.  Take arguments of
   type struct channel *. */

CAMLextern struct channel * caml_open_descriptor_in (int);
CAMLextern struct channel * caml_open_descriptor_out (int);
CAMLextern void caml_close_channel (struct channel *);
CAMLextern int caml_channel_binary_mode (struct channel *);
CAMLextern value caml_alloc_channel(struct channel *chan);

/* High-level I/O operations.
   These functions raise exceptions if errors occur.
   They may run signal handlers.
   The provided channel must *not* be locked:
     these functions lock the channel themselves. */

CAMLextern void caml_flush (struct channel *);
CAMLextern void caml_putch(struct channel*, unsigned char ch);
CAMLextern void caml_putword (struct channel *, uint32_t);
CAMLextern int caml_putblock (struct channel *, char *, intnat);
CAMLextern void caml_really_putblock (struct channel *, char *, intnat);

CAMLextern unsigned char caml_getch(struct channel*);
CAMLextern uint32_t caml_getword (struct channel *);
CAMLextern int caml_getblock (struct channel *, char *, intnat);
CAMLextern intnat caml_really_getblock (struct channel *, char *, intnat);

/* Low-level I/O operations.
   These functions return an error code (0 for success).
   They never raise exceptions nor run signal handlers,
     but may return EINTR if a signal arrives.
     (In such cases, the caller should unlock the channel
      and run caml_process_pending_signals).
   The provided channel must be locked by the caller. */

typedef int io_result;

CAMLextern io_result caml_try_flush (struct channel *);
CAMLextern io_result caml_try_putblock (struct channel *, char *, intnat,
                                        int* written);
CAMLextern io_result caml_try_getblock (struct channel *, char *, intnat,
                                        int* read);

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

#endif /* CAML_INTERNALS */

#endif /* CAML_IO_H */
