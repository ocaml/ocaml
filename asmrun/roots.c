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

/* To walk the memory roots for garbage collection */

#include "memory.h"
#include "major_gc.h"
#include "minor_gc.h"
#include "misc.h"
#include "mlvalues.h"
#include "roots.h"
#include "stack.h"

/* Roots registered from C functions */

value * local_roots = NULL;

struct global_root {
  value * root;
  struct global_root * next;
};
  
static struct global_root * global_roots = NULL;

void (*scan_roots_hook) P((scanning_action)) = NULL;

/* Register a global C root */

void register_global_root(r)
     value * r;
{
  struct global_root * gr;
  gr = (struct global_root *) stat_alloc(sizeof(struct global_root));
  gr->root = r;
  gr->next = global_roots;
  global_roots = gr;
}

/* Un-register a global C root */

void remove_global_root(r)
     value * r;
{
  struct global_root ** gp, * gr;
  for (gp = &global_roots; *gp != NULL; gp = &(*gp)->next) {
    gr = *gp;
    if (gr->root == r) {
      *gp = gr->next;
      stat_free((char *) gr);
      return;
    }
  }
}

/* The hashtable of frame descriptors */

typedef struct {
  unsigned long retaddr;
  short frame_size;
  short num_live;
  short live_ofs[1];
} frame_descr;

static frame_descr ** frame_descriptors = NULL;
static int frame_descriptors_mask;

#define Hash_retaddr(addr) \
  (((unsigned long)(addr) >> 3) & frame_descriptors_mask)

extern long * caml_frametable[];

static void init_frame_descriptors()
{
  long num_descr, tblsize, i, j, len;
  long * tbl;
  frame_descr * d;
  unsigned long h;

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
    (frame_descr **) stat_alloc(tblsize * sizeof(frame_descr *));
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
        (((unsigned long)d +
          sizeof(char *) + sizeof(short) + sizeof(short) +
          sizeof(short) * d->num_live + sizeof(frame_descr *) - 1)
         & -sizeof(frame_descr *));
    }
  }
}

/* Communication with [caml_start_program] and [caml_call_gc]. */

extern value caml_globals[];
extern value gc_entry_regs[];
char * caml_bottom_of_stack = NULL;
unsigned long caml_last_return_address;

/* Structure of markers stored in stack by [callback] to skip C portion
   of stack */

struct callback_link {
  char * bottom_of_stack;
  unsigned long return_address;
};

/* Call [oldify] on all stack roots, C roots and global roots */

void oldify_local_roots ()
{
  char * sp;
  unsigned long retaddr;
  frame_descr * d;
  unsigned long h;
  int i, j, n, ofs;
  short * p;
  value glob;
  value * root, * block;
  struct global_root * gr;

  /* The global roots */
  for (i = 0; caml_globals[i] != 0; i++) {
    glob = caml_globals[i];
    for (j = 0; j < Wosize_val(glob); j++)
      oldify(Field(glob, j), &Field(glob, j));
  }

  /* The stack */
  if (frame_descriptors == NULL) init_frame_descriptors();
  sp = caml_bottom_of_stack;
  retaddr = caml_last_return_address;
  /* A null sp means no more ML stack chunks; stop here. */
  while (sp != NULL) {
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
          root = &gc_entry_regs[ofs >> 1];
        } else {
          root = (value *)(sp + ofs);
        }
        oldify(*root, root);
      }
      /* Move to next frame */
#ifndef Stack_grows_upwards
      sp += d->frame_size;
#else
      sp -= d->frame_size;
#endif
      retaddr = Saved_return_address(sp);
#ifdef Already_scanned
      /* Stop here if the frame has already been scanned during earlier GCs  */
      if (Already_scanned(sp, retaddr)) break;
      /* Mark frame as already scanned */
      Mark_scanned(sp, retaddr);
#endif
    } else {
      /* This marks the top of a stack chunk for an ML callback.
         Skip C portion of stack and continue with next ML stack chunk. */
      retaddr = Callback_link(sp)->return_address;
      sp = Callback_link(sp)->bottom_of_stack;
    }
  }
  /* Local C roots */
  for (block = local_roots; block != NULL; block = (value *) block [1]){
    for (root = block - (long) block [0]; root < block; root++){
      oldify (*root, root);
    }
  }
  /* Global C roots */
  for (gr = global_roots; gr != NULL; gr = gr->next) {
    oldify(*(gr->root), gr->root);
  }
  /* Hook */
  if (scan_roots_hook != NULL) (*scan_roots_hook)(oldify);
}

/* Call [darken] on all roots */

void darken_all_roots ()
{
  do_roots (darken);
}

void do_roots (f)
     scanning_action f;
{
  char * sp;
  unsigned long retaddr;
  frame_descr * d;
  unsigned long h;
  int i, j, n, ofs;
  short * p;
  value glob;
  value * block, * root;
  struct global_root * gr;

  /* The global roots */
  for (i = 0; caml_globals[i] != 0; i++) {
    glob = caml_globals[i];
    for (j = 0; j < Wosize_val(glob); j++)
      f (Field (glob, j), &Field (glob, j));
  }

  /* The stack */
  if (frame_descriptors == NULL) init_frame_descriptors();
  sp = caml_bottom_of_stack;
  retaddr = caml_last_return_address;
  /* A null sp means no more ML stack chunks; stop here. */
  while (sp != NULL) {
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
          f (gc_entry_regs[ofs >> 1], &gc_entry_regs[ofs >> 1]);
        } else {
          f (*(value *)(sp + ofs), (value *)(sp + ofs));
        }
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
      retaddr = Callback_link(sp)->return_address;
      sp = Callback_link(sp)->bottom_of_stack;
    }
  }
  /* Local C roots */
  for (block = local_roots; block != NULL; block = (value *) block [1]){
    for (root = block - (long) block [0]; root < block; root++){
      f (*root, root);
    }
  }
  /* Global C roots */
  for (gr = global_roots; gr != NULL; gr = gr->next) {
    f (*(gr->root), gr->root);
  }
  /* Hook */
  if (scan_roots_hook != NULL) (*scan_roots_hook)(f);
}
