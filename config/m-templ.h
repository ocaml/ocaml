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

/* Processor dependencies */

#define ARCH_SIXTYFOUR

/* Define ARCH_SIXTYFOUR if the processor has a natural word size of 64 bits.
   That is, both sizeof(long) = 8 and sizeof(char *) = 8.
   Otherwise, leave ARCH_SIXTYFOUR undefined. This assumes
   sizeof(long) = sizeof(char *) = 4. */

#define ARCH_BIG_ENDIAN

/* Define ARCH_BIG_ENDIAN if the processor is big endian (the most
   significant byte of an integer stored in memory comes first).
   Leave ARCH_BIG_ENDIAN undefined if the processor is little-endian
   (the least significant byte comes first).
*/

#define ARCH_ALIGN_DOUBLE

/* Define ARCH_ALIGN_DOUBLE if the processor requires doubles to be
   doubleword-aligned. Leave ARCH_ALIGN_DOUBLE undefined if the processor
   supports word-aligned doubles. */

