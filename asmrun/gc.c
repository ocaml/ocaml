#include <stdio.h>
#include <stdlib.h>
#include "misc.h"
#include "mlvalues.h"

char * young_start, * young_ptr, * young_end;
char * old_start, * old_ptr, * old_end;
value ** remembered_start, ** remembered_ptr, ** remembered_end;

/* Heap initialization */

int young_size = 32 * sizeof(value) * 1024; /* 128K / 256K */
int old_size = 256 * sizeof(value) * 1024;  /* 1M / 2M */
int remembered_size = 4096;

void init_heap()
{
  young_start = malloc(young_size);
  old_start = malloc(old_size);
  remembered_start =
    (value **) malloc(remembered_size * sizeof(value *));
  if (young_start == NULL ||
      old_start == NULL ||
      remembered_start == NULL) {
    fprintf(stderr, "Cannot allocate initial heap\n");
    exit(2);
  }
  young_end = young_start + young_size;
  young_ptr = young_end;
  old_end = old_start + old_size;
  old_ptr = old_start;
  remembered_end = remembered_start + remembered_size;
  remembered_ptr = remembered_start;
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
  (((unsigned long)(addr) >> 2) & frame_descriptors_mask)

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
     4 times the number of descriptors */
  tblsize = 4;
  while (tblsize < 4 * num_descr) tblsize *= 2;

  /* Allocate the hash table */
  frame_descriptors =
    (frame_descr **) malloc(tblsize * sizeof(frame_descr *));
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

/* Copy an object (but not its descendents) and overwrite it with
   its new location */

#define Forward_mask 0x100

#if defined(__GNUC__) && !defined(DEBUG)
static inline
#else
static
#endif
void copy_obj(addr)
     value * addr;
{
  value v, res;
  header_t hdr, size, ofs, i;

  v = *addr;
  if (Is_int(v) || (char *) v <= young_start || (char *) v > young_end)
    return;
  hdr = Header_val(v);
  if (hdr & Forward_mask) {     /* Already copied? */
    res = Field(v, 0);          /* Forwarding pointer is in field 0 */
  } else if (Tag_header(hdr) != Infix_tag) {
    size = Size_header(hdr);
    res = (value) (old_ptr + sizeof(header_t));
    old_ptr += sizeof(header_t) + size * sizeof(value);
    Header_val(res) = hdr & ~Modified_mask;
    for (i = 0; i < size; i++)
      Field(res, i) = Field(v, i);
    Header_val(v) = hdr | Forward_mask; /* Set forward mark */
    Field(v, 0) = res;                  /* Store forwarding pointer */
  } else {
    ofs = Size_header(hdr) * sizeof(value);
    v -= ofs;
    hdr = Header_val(v);
    if (hdr & Forward_mask) {
      res = Field(v, 0);
    } else {
      size = Size_header(hdr);
      res = (value) (old_ptr + sizeof(header_t));
      Header_val(res) = hdr & ~Modified_mask;
      old_ptr += sizeof(header_t) + size * sizeof(value);
      for (i = 0; i < size; i++)
        Field(res, i) = Field(v, i);
      Header_val(v) = hdr | Forward_mask;
      Field(v, 0) = res;
    }
    res += ofs;
  }
  *addr = res;
}

/* Machine-dependent stack frame accesses */

#ifdef alpha
#define Saved_return_address(sp) *((long *)(sp - 8))
#define Already_scanned(sp, retaddr) (retaddr & 1)
#define Mark_scanned(sp, retaddr) (*((long *)(sp - 8)) = retaddr | 1)
/** #define Already_scanned(sp, retaddr) 0 **/
/** #define Mark_scanned(sp, retaddr) **/
#endif

extern value * caml_globals[];
extern char * caml_bottom_of_stack, * caml_top_of_stack;
extern unsigned long caml_last_return_address;
extern value gc_entry_regs[];

/* Copy everything in the minor heap */

static void minor_collection()
{
  char * scan_ptr, * sp;
  unsigned long retaddr;
  frame_descr * d;
  unsigned long h;
  int i, n, ofs;
  short * p;
  value v;
  header_t hdr, size;
  value * root, ** rem;

  scan_ptr = old_ptr;

  /* Copy the global values */
  for (i = 0; caml_globals[i] != 0; i++) copy_obj(caml_globals[i]);

  /* Stack roots */
  if (frame_descriptors == NULL) init_frame_descriptors();
  sp = caml_bottom_of_stack;
  retaddr = caml_last_return_address;

  while (sp < caml_top_of_stack) {
    /* Find the descriptor corresponding to the return address */
    h = Hash_retaddr(retaddr);
    while(1) {
      d = frame_descriptors[h];
      if (d->retaddr == retaddr) break;
      h = (h+1) & frame_descriptors_mask;
    }
    /* Scan the roots in this frame */
    for (p = d->live_ofs, n = d->num_live; n > 0; n--, p++) {
      ofs = *p;
      if (ofs >= 0) {
        Assert(ofs < d->frame_size);
        root = (value *)(sp + ofs);
      } else {
        Assert(ofs >= -32);
        root = &gc_entry_regs[-ofs-1];
      }
      copy_obj(root);
    }
    /* Move to next frame */
    sp += d->frame_size;
    retaddr = Saved_return_address(sp);
    /* Stop here if already scanned */
    if (Already_scanned(sp, retaddr)) break;
    /* Mark frame as already scanned */
    Mark_scanned(sp, retaddr);
  }

  /* Scan the remembered set */
  for (rem = remembered_start; rem < remembered_ptr; rem++) {
    v = **rem;
    hdr = Header_val(v);
    if (hdr < No_scan_tag) {
      size = Size_header(hdr);
      for (i = 0; i < size; i++) copy_obj(&Field(v, i));
    }
    Header_val(v) &= ~Modified_mask;
  }

  /* Finish the copying */

  while (scan_ptr < old_ptr) {
    v = (value) (scan_ptr + sizeof(header_t));
    hdr = Header_val(v);
    size = Size_header(hdr);
    if (Tag_header(hdr) < No_scan_tag) {
      for (i = 0; i < size; i++) copy_obj(&Field(v, i));
    }
    scan_ptr += sizeof(header_t) + size * sizeof(value);
  }

  /* Reset allocation pointers */
  young_ptr = young_end;
  remembered_ptr = remembered_start;
}

/* Garbage collection */

void garbage_collection(request)
     unsigned long request;
{
  char * initial_old_ptr;

  fprintf(stderr, "<"); fflush(stderr);
#ifdef DEBUG
  Assert(young_ptr <= young_end);
  Assert(young_ptr < young_start);
  Assert(young_ptr + request >= young_start);
  check_globals();
  check_heap(young_ptr + request, young_end);
  check_heap(old_start, old_ptr);
#endif
  if (old_end - old_ptr < young_size) {
    fprintf(stderr, "reallocating old generation "); fflush(stderr);
    old_start = malloc(old_size);
    if (old_start == NULL) {
      fprintf(stderr, "Cannot extend heap\n");
      exit(2);
    }
    old_end = old_start + old_size;
    old_ptr = old_start;
  }
  initial_old_ptr = old_ptr;
  minor_collection();
#ifdef DEBUG
  check_globals();
  check_heap(old_start, old_ptr);
#endif
  young_ptr -= request;
  fprintf(stderr, "%d%%>", ((old_ptr - initial_old_ptr) * 100) / young_size);
  fflush(stderr);
}

/* Reallocate remembered set */

void realloc_remembered()
{
  int used = remembered_ptr - remembered_start;
  remembered_size *= 2;
  remembered_start =
    (value **) realloc(remembered_start, remembered_size);
  if (remembered_start == NULL) {
    fprintf(stderr, "Cannot reallocate remembered set\n");
    exit(2);
  }
  remembered_end = remembered_start + remembered_size;
  remembered_ptr = remembered_start + used;
}
