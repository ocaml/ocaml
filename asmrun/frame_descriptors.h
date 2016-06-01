#ifndef CAML_FRAME_DESCRIPTORS_H
#define CAML_FRAME_DESCRIPTORS_H

#include "caml/mlvalues.h"
#include "caml/roots.h"

/* Structure of frame descriptors */

typedef struct {
  uintnat retaddr;
  unsigned short frame_size;
  unsigned short num_live;
  unsigned short live_ofs[1];
} frame_descr;


void caml_init_frame_descriptors(void);

void caml_register_frametable(intnat *table);

void caml_scan_stack_roots(scanning_action f,
                           char* sp, uintnat retaddr, value* regs);

frame_descr* caml_find_frame_descr(uintnat pc);

#endif /* CAML_FRAME_DESCRIPTORS_H */
