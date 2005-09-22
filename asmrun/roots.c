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

/* To walk the memory roots for garbage collection */

#include "finalise.h"
#include "globroots.h"
#include "memory.h"
#include "major_gc.h"
#include "minor_gc.h"
#include "misc.h"
#include "mlvalues.h"
#include "stack.h"
#include "roots.h"

/* Roots registered from C functions */

struct caml__roots_block *caml_local_roots = NULL;

void (*caml_scan_roots_hook) (scanning_action) = NULL;

/* The hashtable of frame descriptors */

typedef struct {
  uintnat retaddr;
  short frame_size;
  short num_live;
  short live_ofs[1];
} frame_descr;

static frame_descr ** frame_descriptors = NULL;
static int frame_descriptors_mask;

#define Hash_retaddr(addr) \
  (((uintnat)(addr) >> 3) & frame_descriptors_mask)

static void init_frame_descriptors(void)
{
  intnat num_descr, tblsize, i, j, len;
  intnat * tbl;
  frame_descr * d;
  uintnat h;

  /* Count the frame descriptors */
  num_descr = 0;
  for (i = 0; caml_frametable[i] != 0; i++)
    num_descr += *(caml_frametable[i]);

  /* The size of the hashtable is a power of 2 greater or equal to
     2 times the number of descriptors */
  tblsize = 4;
  while (tblsize < 2 * num_descr) tblsize *= 2;

  /* Allocate the hash table */
  frame_descriptors =
    (frame_descr **) caml_stat_alloc(tblsize * sizeof(frame_descr *));
  for (i = 0; i < tblsize; i++) frame_descriptors[i] = NULL;
  frame_descriptors_mask = tblsize - 1;

  /* Fill the hash table */
  for (i = 0; caml_frametable[i] != 0; i++) {
    tbl = caml_frametable[i];
    len = *tbl;
    d = (frame_descr *)(tbl + 1);
    for (j = 0; j < len; j++) {
      h = Hash_retaddr(d->retaddr);
      while (frame_descriptors[h] != NULL) {
        h = (h+1) & frame_descriptors_mask;
      }
      frame_descriptors[h] = d;
      d = (frame_descr *)
        (((uintnat)d +
          sizeof(char *) + sizeof(short) + sizeof(short) +
          sizeof(short) * d->num_live + sizeof(frame_descr *) - 1)
         & -sizeof(frame_descr *));
    }
  }
}

/* Communication with [caml_start_program] and [caml_call_gc]. */

char * caml_bottom_of_stack = NULL; /* no stack initially */
uintnat caml_last_return_address = 1; /* not in Caml code initially */
value * caml_gc_regs;
intnat caml_globals_inited = 0;
static intnat caml_globals_scanned = 0;

/* Call [caml_oldify_one] on (at least) all the roots that point to the minor
   heap. */
void caml_oldify_local_roots (void)
{
  char * sp;
  uintnat retaddr;
  value * regs;
  frame_descr * d;
  uintnat h;
  int i, j, n, ofs;
  short * p;
  value glob;
  value * root;
  struct global_root * gr;
  struct caml__roots_block *lr;

  /* The global roots */
  for (i = caml_globals_scanned;
       i <= caml_globals_inited && caml_globals[i] != 0;
       i++) {
    glob = caml_globals[i];
    for (j = 0; j < Wosize_val(glob); j++){
      Oldify (&Field (glob, j));
    }
  }
  caml_globals_scanned = caml_globals_inited;

  /* The stack and local roots */
  if (frame_descriptors == NULL) init_frame_descriptors();
  sp = caml_bottom_of_stack;
  retaddr = caml_last_return_address;
  regs = caml_gc_regs;
  if (sp != NULL) {
    while (1) {
      /* Find the descriptor corresponding to the return address */
      h = Hash_retaddr(retaddr);
      while(1) {
        d = frame_descriptors[h];
        if (d->retaddr == retaddr) break;
        h = (h+1) & frame_descriptors_mask;
      }
      if (d->frame_size >= 0) {
        /* Scan the roots in this frame */
        for (p = d->live_ofs, n = d->num_live; n > 0; n--, p++) {
          ofs = *p;
          if (ofs & 1) {
            root = regs + (ofs >> 1);
          } else {
            root = (value *)(sp + ofs);
          }
          Oldify (root);
        }
        /* Move to next frame */
#ifndef Stack_grows_upwards
        sp += d->frame_size;
#else
        sp -= d->frame_size;
#endif
        retaddr = Saved_return_address(sp);
#ifdef Already_scanned
        /* Stop here if the frame has been scanned during earlier GCs  */
        if (Already_scanned(sp, retaddr)) break;
        /* Mark frame as already scanned */
        Mark_scanned(sp, retaddr);
#endif
      } else {
        /* This marks the top of a stack chunk for an ML callback.
           Skip C portion of stack and continue with next ML stack chunk. */
        struct caml_context * next_context = Callback_link(sp);
        sp = next_context->bottom_of_stack;
        retaddr = next_context->last_retaddr;
        regs = next_context->gc_regs;
        /* A null sp means no more ML stack chunks; stop here. */
        if (sp == NULL) break;
      }
    }
  }
  /* Local C roots */
  for (lr = caml_local_roots; lr != NULL; lr = lr->next) {
    for (i = 0; i < lr->ntables; i++){
      for (j = 0; j < lr->nitems; j++){
        root = &(lr->tables[i][j]);
        Oldify (root);
      }
    }
  }
  /* Global C roots */
  for (gr = caml_global_roots.forward[0]; gr != NULL; gr = gr->forward[0]) {
    Oldify (gr->root);
  }
  /* Finalised values */
  caml_final_do_young_roots (&caml_oldify_one);
  /* Hook */
  if (caml_scan_roots_hook != NULL) (*caml_scan_roots_hook)(caml_oldify_one);
}

/* Call [darken] on all roots */

void caml_darken_all_roots (void)
{
  caml_do_roots (caml_darken);
}

void caml_do_roots (scanning_action f)
{
  int i, j;
  value glob;
  struct global_root * gr;

  /* The global roots */
  for (i = 0; caml_globals[i] != 0; i++) {
    glob = caml_globals[i];
    for (j = 0; j < Wosize_val(glob); j++)
      f (Field (glob, j), &Field (glob, j));
  }
  /* The stack and local roots */
  if (frame_descriptors == NULL) init_frame_descriptors();
  caml_do_local_roots(f, caml_bottom_of_stack, caml_last_return_address,
                      caml_gc_regs, caml_local_roots);
  /* Global C roots */
  for (gr = caml_global_roots.forward[0]; gr != NULL; gr = gr->forward[0]) {
    f(*(gr->root), gr->root);
  }
  /* Finalised values */
  caml_final_do_strong_roots (f);
  /* Hook */
  if (caml_scan_roots_hook != NULL) (*caml_scan_roots_hook)(f);
}

void caml_do_local_roots(scanning_action f, char * bottom_of_stack,
                         uintnat last_retaddr, value * gc_regs,
                         struct caml__roots_block * local_roots)
{
  char * sp;
  uintnat retaddr;
  value * regs;
  frame_descr * d;
  uintnat h;
  int i, j, n, ofs;
  short * p;
  value * root;
  struct caml__roots_block *lr;

  sp = bottom_of_stack;
  retaddr = last_retaddr;
  regs = gc_regs;
  if (sp != NULL) {
    while (1) {
      /* Find the descriptor corresponding to the return address */
      h = Hash_retaddr(retaddr);
      while(1) {
        d = frame_descriptors[h];
        if (d->retaddr == retaddr) break;
        h = (h+1) & frame_descriptors_mask;
      }
      if (d->frame_size >= 0) {
        /* Scan the roots in this frame */
        for (p = d->live_ofs, n = d->num_live; n > 0; n--, p++) {
          ofs = *p;
          if (ofs & 1) {
            root = regs + (ofs >> 1);
          } else {
            root = (value *)(sp + ofs);
          }
          f (*root, root);
        }
        /* Move to next frame */
#ifndef Stack_grows_upwards
        sp += d->frame_size;
#else
        sp -= d->frame_size;
#endif
        retaddr = Saved_return_address(sp);
#ifdef Mask_already_scanned
        retaddr = Mask_already_scanned(retaddr);
#endif
      } else {
        /* This marks the top of a stack chunk for an ML callback.
           Skip C portion of stack and continue with next ML stack chunk. */
        struct caml_context * next_context = Callback_link(sp);
        sp = next_context->bottom_of_stack;
        retaddr = next_context->last_retaddr;
        regs = next_context->gc_regs;
        /* A null sp means no more ML stack chunks; stop here. */
        if (sp == NULL) break;
      }
    }
  }
  /* Local C roots */
  for (lr = local_roots; lr != NULL; lr = lr->next) {
    for (i = 0; i < lr->ntables; i++){
      for (j = 0; j < lr->nitems; j++){
        root = &(lr->tables[i][j]);
        f (*root, root);
      }
    }
  }
}
