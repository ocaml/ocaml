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

/* exec.h : format of executable bytecode files */

/*  offset 0 --->  initial junk
                   path to runtime (if needed)
                   code block
                   names of primitives
                   data block
                   symbol table
                   debug infos
                   trailer
 end of file --->
*/

/* Structure of the trailer.
   Sizes are 32-bit unsigned integers, big endian */

#define TRAILER_SIZE (6*4+12)

struct exec_trailer {
  unsigned int path_size;      /* Length of path to runtime (w. final \n) */
  unsigned int code_size;      /* Size of the code block (in bytes) */
  unsigned int prim_size;      /* Size of the primitive table (in bytes) */
  unsigned int data_size;      /* Size of the global data table (bytes) */
  unsigned int symbol_size;    /* Size of the symbol table (bytes) */
  unsigned int debug_size;     /* Size of the debug infos (bytes) */
  char magic[12];              /* A magic string */
};

/* Magic number for this release */

#define EXEC_MAGIC "Caml1999X004"

