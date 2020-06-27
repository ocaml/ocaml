/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*                     James E. Fehrle, unaffiliated                      */
/*                                                                        */
/*   Copyright 2020 James E. Fehrle                                       */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

// ELF executable file format (partial)

#ifndef CAML_ELF_H
#define CAML_ELF_H

#ifdef CAML_INTERNALS

#include "config.h"
#include "stack.h"

// ELF file format:
// see https://en.wikipedia.org/wiki/Executable_and_Linkable_Format
// and http://www.cs.yale.edu/homes/aspnes/pinewiki/attachments/...
//       ELF(20)format/ELF_format.pdf

#ifdef ARCH_SIXTYFOUR
#define ELF_Uint uint64_t
#define ELF_INT_PRINTF_FORMAT ARCH_INT64_PRINTF_FORMAT
#else
#define ELF_Uint uint32_t
#define ELF_INT_PRINTF_FORMAT ARCH_INT32_PRINTF_FORMAT
#endif

#define ELF_Addr  ELF_Uint
#define ELF_Off   ELF_Uint
#define ELF_Int4  uint32_t
#define ELF_Int2  uint16_t

#define EINIDENT    16
#define EI_CLASS    0x4
#define EI_DATA     0x5
#define EI_VERSION  0x6

typedef struct {
  unsigned char e_ident[EINIDENT]; /* ELF identification */
  ELF_Int2      e_type; /* Object file type */
  ELF_Int2      e_machine; /* Machine type */
  ELF_Int4      e_version; /* Object file version */
  ELF_Addr      e_entry; /* Entry point address */
  ELF_Off       e_phoff; /* Program header offset */
  ELF_Off       e_shoff; /* Section header offset */
  ELF_Int4      e_flags; /* Processor-specific flags */
  ELF_Int2      e_ehsize; /* ELF header size */
  ELF_Int2      e_phentsize; /* Size of program header entry */
  ELF_Int2      e_phnum; /* Number of program header entries */
  ELF_Int2      e_shentsize; /* Size of section header entry */
  ELF_Int2      e_shnum; /* Number of section header entries */
  ELF_Int2      e_shstrndx; /* Section name string table index */
} Elf_File_hdr;

typedef struct {
  ELF_Int4      sh_name; /* Section name */
  ELF_Int4      sh_type; /* Section type */
  ELF_Uint      sh_flags; /* Section attributes */
  ELF_Addr      sh_addr; /* Virtual address in memory */
  ELF_Off       sh_offset; /* Offset in file */
  ELF_Uint      sh_size; /* Size of section */
  ELF_Int4      sh_link; /* Link to other section */
  ELF_Int4      sh_info; /* Miscellaneous information */
  ELF_Uint      sh_addralign; /* Address alignment boundary */
  ELF_Uint      sh_entsize; /* Size of entries, if section has table */
} Elf_Section_hdr;

#ifdef ARCH_SIXTYFOUR
typedef struct {
  ELF_Int4      st_name; /* Symbol name */
  unsigned char st_info; /* Type and Binding attributes */
  unsigned char st_other; /* Reserved */
  ELF_Int2      st_shndx; /* Section table index */
  ELF_Addr      st_value; /* Symbol value */
  ELF_Uint      st_size; /* Size of object (e.g., common) */
} Elf_Sym;

#else
typedef struct {
  ELF_Int4      st_name; /* Symbol name */
  ELF_Addr      st_value; /* Symbol value */
  ELF_Int4      st_size; /* Size of object (e.g., common) */
  unsigned char st_info; /* Type and Binding attributes */
  unsigned char st_other; /* Reserved */
  ELF_Int2      st_shndx; /* Section table index */
} Elf_Sym;
#endif

typedef struct {
  int frame_item;
  int frame_num;
  frame_descr *cur_descr;
  int min;
} Stack_roots;

void caml_do_print_reachable(long);
int caml_do_field_path(value, int *, int);

#endif /* CAML_INTERNALS */

#endif /* CAML_ELF_H */
