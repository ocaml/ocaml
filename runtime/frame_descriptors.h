#ifndef CAML_FRAME_DESCRIPTORS_H
#define CAML_FRAME_DESCRIPTORS_H

#include "caml/config.h"

#define Hash_retaddr(addr, mask)                          \
  (((uintnat)(addr) >> 3) & (mask))

/* Structure of frame descriptors */

typedef struct {
  uintnat retaddr;
  unsigned short frame_size;
  unsigned short num_live;
  unsigned short live_ofs[1 /* num_live */];
  /*
    If frame_size & 1, then debug info follows:
  uint32_t debug_info_offset;
    Debug info is stored as a relative offset to a debuginfo structure. */
} frame_descr;

/* Used to compute offsets in frame tables.
   ty must have power-of-2 size */
#define Align_to(p, ty) \
  (void*)(((uintnat)(p) + sizeof(ty) - 1) & -sizeof(ty))


void caml_init_frame_descriptors(void);
void caml_register_frametable(intnat *table);

typedef struct {
  frame_descr** descriptors;
  int mask;
} caml_frame_descrs;

caml_frame_descrs caml_get_frame_descrs(void);

/* Find the current table of frame descriptors.
   The resulting structure is only valid until the next GC */
frame_descr* caml_find_frame_descr(caml_frame_descrs fds, uintnat pc);

#endif /* CAML_FRAME_DESCRIPTORS_H */
