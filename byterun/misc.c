/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*         Xavier Leroy and Damien Doligez, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  Automatique.  Distributed only by permission.                      */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#include <stdio.h>
#include "config.h"
#include "misc.h"
#ifdef HAS_UI
#include "ui.h"
#endif

#ifdef DEBUG

void failed_assert (expr, file, line)
     char *expr, *file;
     int line;
{
  fprintf (stderr, "Assertion failed: %s; file %s; line %d\n",
	   expr, file, line);
  exit (100);
}

static unsigned long seed = 0x12345;

unsigned long not_random ()
{
  seed = seed * 65537 + 12345;
  return seed;
}

#endif

int verb_gc;

void gc_message (msg, arg)
     char *msg;
     unsigned long arg;
{
  if (verb_gc){
#ifdef HAS_UI
    ui_print_stderr(msg, (void *) arg);
#else
    fprintf (stderr, msg, arg);
    fflush (stderr);
#endif
  }
}

void fatal_error (msg)
     char * msg;
{
#ifdef HAS_UI
  ui_print_stderr("%s", msg);
  ui_exit (2);
#else
  fprintf (stderr, "%s", msg);
  exit(2);
#endif
}

void fatal_error_arg (fmt, arg)
     char * fmt, * arg;
{
#ifdef HAS_UI
  ui_print_stderr(fmt, arg);
  ui_exit (2);
#else
  fprintf (stderr, fmt, arg);
  exit(2);
#endif
}

#ifdef USING_MEMMOV

/* This should work on 64-bit machines as well as 32-bit machines.
   It assumes a long is the natural size for memory reads and writes.
*/
void memmov (dst, src, length)
     char *dst, *src;
     unsigned long length;
{
  unsigned long i;

  if ((unsigned long) dst <= (unsigned long) src){

      /* Copy in ascending order. */
    if (((unsigned long) src - (unsigned long) dst) % sizeof (long) != 0){

        /* The pointers are not equal modulo sizeof (long).
           Copy byte by byte. */
      for (; length != 0; length--){
	*dst++ = *src++;
      }
    }else{

        /* Copy the first few bytes. */
      i = (unsigned long) dst % sizeof (long);
      if (i != 0){
	i = sizeof (long) - i;              /* Number of bytes to copy. */
	if (i > length) i = length;         /* Never copy more than length.*/
	for (; i != 0; i--){
	  *dst++ = *src++; --length;
	}
      }                    Assert ((unsigned long) dst % sizeof (long) == 0);
                           Assert ((unsigned long) src % sizeof (long) == 0);

      /* Then copy as many entire words as possible. */
      for (i = length / sizeof (long); i > 0; i--){
	*(long *) dst = *(long *) src;
	dst += sizeof (long); src += sizeof (long);
      }

      /* Then copy the last few bytes. */
      for (i = length % sizeof (long); i > 0; i--){
	*dst++ = *src++;
      }
    }
  }else{                                       /* Copy in descending order. */
    src += length; dst += length;
    if (((unsigned long) dst - (unsigned long) src) % sizeof (long) != 0){

        /* The pointers are not equal modulo sizeof (long).
	   Copy byte by byte. */
      for (; length > 0; length--){
	*--dst = *--src;
      }
    }else{

        /* Copy the first few bytes. */
      i = (unsigned long) dst % sizeof (long);
      if (i > length) i = length;           /* Never copy more than length. */
      for (; i > 0; i--){
	*--dst = *--src; --length;
      }

        /* Then copy as many entire words as possible. */
      for (i = length / sizeof (long); i > 0; i--){
	dst -= sizeof (long); src -= sizeof (long);
	*(long *) dst = *(long *) src;
      }

        /* Then copy the last few bytes. */
      for (i = length % sizeof (long); i > 0; i--){
	*--dst = *--src;
      }
    }
  }
}

#endif /* USING_MEMMOV */

char *aligned_malloc (size, modulo, block)
     asize_t size;
     int modulo;
     void **block;      /* output */
{
  char *raw_mem;
  unsigned long aligned_mem;
                                                 Assert (modulo < Page_size);
  raw_mem = (char *) malloc (size + Page_size);
  if (raw_mem == NULL) return NULL;
  *block = raw_mem;
  raw_mem += modulo;		/* Address to be aligned */
  aligned_mem = (((unsigned long) raw_mem / Page_size + 1) * Page_size);
  return (char *) (aligned_mem - modulo);
}
