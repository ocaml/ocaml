/* exec.h : format of executable bytecode files */

/*  offset 0 --->  initial junk
                   code block
                   data block
                   symbol table
                   debug infos
                   trailer
 end of file --->
*/

/* Structure of the trailer: four 32-bit, unsigned integers, big endian */

#define TRAILER_SIZE (4*4+12)

struct exec_trailer {
  unsigned long code_size;      /* Size of the code block (in bytes) */
  unsigned long data_size;      /* Size of the global data table (bytes) */
  unsigned long symbol_size;    /* Size of the symbol table (bytes) */
  unsigned long debug_size;     /* Size of the debug infos (bytes) */
  char magic[12];               /* A magic string */
};

/* Magic number for this release */

#define EXEC_MAGIC "Caml1999X001"

