#ifndef CAML_FRAME_DESCRIPTORS_H
#define CAML_FRAME_DESCRIPTORS_H

#include "caml/mlvalues.h"
#include "caml/roots.h"

#define Hash_retaddr(addr, mask)                          \
  (((uintnat)(addr) >> 3) & (mask))

/* Structure of frame descriptors */

typedef struct {
  uintnat retaddr;
  unsigned short frame_size;
  unsigned short num_live;
  unsigned short live_ofs[1];
} frame_descr;


void caml_init_frame_descriptors(void);

void caml_register_frametable(intnat *table);

frame_descr* caml_find_frame_descr(uintnat pc);

CAMLextern value caml_frame_descriptor_table;

#endif /* CAML_FRAME_DESCRIPTORS_H */
