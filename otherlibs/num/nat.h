/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1999 Institut National de Recherche en Informatique et   */
/*  en Automatique.  Distributed only by permission.                   */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

/* Nats are represented as unstructured blocks with tag Nat_tag. */

#define Nat_tag Abstract_tag    /* works OK with equal because no other
                                   object uses that tag yet. */

#define Bignum_val(nat) ((BigNum) nat)

