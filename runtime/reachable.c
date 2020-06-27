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
#define CAML_INTERNALS

#include "caml/alloc.h"
#include "caml/backtrace_prim.h"
#include "caml/elf.h"
#include "caml/finalise.h"
#include "caml/globroots.h"
#include "caml/roots.h"
#include "caml/stack.h"
#include "caml/memprof.h"
#include "caml/osdeps.h"
#include <stdio.h>
#include <string.h>

/* Linked-list */

typedef struct dyn_root_link {
  void *data;
  char *symname;
  struct dyn_root_link *next;
} dyn_root_link;
extern dyn_root_link * caml_dyn_globals;

#define iter_list(list,lnk) \
  for (lnk = list; lnk != NULL; lnk = lnk->next)

Stack_roots caml_stack_roots;  // for access by caml_do_local_roots

extern CAMLprim value caml_obj_reachable_words(value v);
extern char_os * caml_exe_name;

typedef struct {
  char *buf;            // malloced copy of the executable file
  Elf_Sym *symtab;      // symbol table (".symtab" section)
  ELF_Uint symtab_num_ents; // # of entries in symbol table
  char *strtab;         // string table (".strtab" section)
  ELF_Uint relocation;   // difference between in-memory and symtab addresses
  char *shrstrtab;
} Elf_info;

static ELF_Uint get_elf_symbol_by_name(Elf_info *elf_info, char *name) {
  int i;

  for (i = 0; i < elf_info->symtab_num_ents; i++) {
    Elf_Sym *sym_ent = &elf_info->symtab[i];
    if (strcmp(elf_info->strtab + sym_ent->st_name, name) == 0)
      return sym_ent->st_value;
  }

  return -1; // not found
}

static Elf_Section_hdr *find_section(Elf_info *elf_info, char *name) {
  char *buf = elf_info->buf;
  Elf_File_hdr *fh = (Elf_File_hdr *)buf;
  Elf_Section_hdr *section_hdrs = (Elf_Section_hdr *)(buf + fh->e_shoff);
  int sect_num;

  for (sect_num = 1; sect_num < fh->e_shnum; sect_num++) {
    Elf_Section_hdr *sh = &section_hdrs[sect_num];
    char *sh_name = elf_info->shrstrtab + sh->sh_name;
    if (strcmp(sh_name, name) == 0)
      return &section_hdrs[sect_num];
  }
  return NULL;
}

static int caml_reloc_sym;

// read the ELF format object code file
static char *elf_setup(Elf_info *elf_info) {
  char *rv = "";
  FILE *exe_file = NULL;
  char *buf = NULL;
  long len;
  size_t read_len;
  Elf_File_hdr *fh;
  const char ELF[] = { 0x7F, 'E', 'L' ,'F' };
  Elf_Section_hdr *section_hdrs, *shrstrtab_hdr, *sh;
  int is32Bit, isLittleEndian, my_endian = 0;
  char *p;
  ELF_Uint symtab_entsize,reloc_sym;

  do {
    exe_file = fopen_os(caml_exe_name, T("rb"));
    if (exe_file == NULL) {
      rv = "fopen_os failure";
      break;
    }
    if (fseek(exe_file, 0L, SEEK_END) != 0) {
      rv = "fseek failure";
      break;
    }
    len = ftell(exe_file);
    if (len == -1) {
      rv = "ftell failure";
      break;
    }
    if (fseek(exe_file, 0L, SEEK_SET) != 0) {
      rv = "2nd fseek failure";
      break;
    }
    buf = malloc(len);
    if (buf == NULL) {
      rv = "malloc failure";
      break;
    }
    elf_info->buf = buf;
    read_len = fread(buf, sizeof(unsigned char), len, exe_file);
    if (read_len != len) {
      rv = "fread failure";
      break;
    }

    fh = (Elf_File_hdr *)buf;
    if (memcmp(fh->e_ident, ELF, sizeof(ELF)) != 0) {
      rv = "not an ELF file";
      break;
    }

    is32Bit = fh->e_ident[EI_CLASS] == 1;
    if (is32Bit ^ (sizeof(ELF_Uint) == 4)) {
      rv = "32/64-bit mismatch in executable file";
      break;
    }

    p = (char *)&my_endian;
    *p = 1;
    isLittleEndian = fh->e_ident[EI_DATA] == 1;
    if (isLittleEndian ^ (my_endian == 1)) {
      rv = "endianness mismatch in executable file";
      break;
    }

    if (fh->e_ident[EI_VERSION] != 1 || fh->e_version != 1) {
      rv = "not ELF V1";
      break;
    }

    section_hdrs = (Elf_Section_hdr *)(buf + fh->e_shoff);
    shrstrtab_hdr = &section_hdrs[fh->e_shstrndx];
    elf_info->shrstrtab = buf + shrstrtab_hdr->sh_offset;

    sh = find_section(elf_info, ".strtab");
    if (sh == NULL) {
      rv = "Missing .strtab section";
      break;
    }
    elf_info->strtab = buf + sh->sh_offset;

    sh = find_section(elf_info, ".symtab");
    if (sh == NULL) {
      rv = "Missing .symtab section";
      break;
    }
    elf_info->symtab = (Elf_Sym *)(buf + sh->sh_offset);
    symtab_entsize = sh->sh_entsize;
    elf_info->symtab_num_ents = sh->sh_size / symtab_entsize;

    if (symtab_entsize != sizeof (Elf_Sym)) {
      rv = "Symtab entry size mismatch";
      break;
    }

    reloc_sym = caml_reloc_sym;  // avoid "unused variable" warning
    reloc_sym = get_elf_symbol_by_name(elf_info, "caml_reloc_sym");
    if (reloc_sym == -1) {
      rv = "Can't find 'caml_reloc_sym' symbol";
      break;
    }
    elf_info->relocation = ((ELF_Uint)&caml_reloc_sym) - reloc_sym;
  } while (0);

  if (exe_file != NULL)
    fclose(exe_file);
  if (strcmp(rv, "") != 0 && buf != NULL) {
    elf_info->buf = NULL;
    free(buf);
  }
  return rv;
}

static char *get_elf_symbol_by_address(Elf_info *elf_info, value *addr) {
  ELF_Uint sym_unrelocated;
  Elf_Sym *sym_ent;
  int i;

  if (elf_info->buf == NULL) {
    printf("call elf_info_setup before calling "
        "get_elf_symbol_by_address\n");
    return "???";
  }

  sym_unrelocated = (ELF_Uint)addr - elf_info->relocation;
  for (i = 0; i < elf_info->symtab_num_ents; i++) {
    sym_ent = &elf_info->symtab[i];
    if (sym_ent->st_value == sym_unrelocated)
      return strdup(elf_info->strtab + sym_ent->st_name);
  }

  return "???";  // not found
}

static struct {
  ELF_Uint min;          // minimum number of reachable words
  char *header_string;  // the header string (per section)
  int printed_header;   // boolean; 1 = header has been printed
  int item_num;         // simple counter but not very meaningful
} print_roots;

// callback routine
static void root_report (value v, value *p /* not used */) {
  ELF_Uint reachable = Long_val(caml_obj_reachable_words(v));
  if (reachable >= print_roots.min) {
    if (!print_roots.printed_header) {
      printf("\n%s:\n", print_roots.header_string);
      print_roots.printed_header = 1;
    }
    printf("%10" ELF_INT_PRINTF_FORMAT "d item  %3d\n",
        reachable, print_roots.item_num);
  }
  print_roots.item_num++;
}

extern void caml_print_location(FILE *, struct caml_loc_info * li, int index);

// callback routine
static void stack_report (value v, value *p /* not used */) {
  if (caml_stack_roots.cur_descr != NULL) {
    ELF_Uint reachable = Long_val(caml_obj_reachable_words(v));
    if (reachable >= caml_stack_roots.min) {
      if (!print_roots.printed_header) {
        printf("\n");
        print_roots.printed_header = 1;
      }
      if (caml_stack_roots.frame_item == 0) {
        debuginfo dbi = caml_debuginfo_extract(caml_stack_roots.cur_descr);
        struct caml_loc_info li;
        caml_debuginfo_location(dbi, &li);
        caml_print_location(stdout, &li, caml_stack_roots.frame_item+1);
        caml_stack_roots.frame_num++;
      }
      printf("%10" ELF_INT_PRINTF_FORMAT "d field %3d (frame %d)\n", reachable,
          caml_stack_roots.frame_item, caml_stack_roots.frame_num-1);
    }
  }
}

static void do_stack_roots(long min) {
  caml_stack_roots.min = min;
  caml_stack_roots.frame_num = 0;
  caml_do_local_roots(stack_report, Caml_state->bottom_of_stack,
                      Caml_state->last_return_address, Caml_state->gc_regs,
                      Caml_state->local_roots);

}

/* Reports how much memory is reachable from each heap root for those that
reach at least min words.  This is the main code implementing
Gc.print_reachable.

Reads the executable file, which must be in ELF format, to get symbol names
for global roots.  The symbol names incorporate the source file name.  The
symbol names for dynamic roots is saved at load time in
caml_register_dyn_global.

Accessing roots is modelled on caml_do_roots.

Stack frames are printed only when one of the fields has more than the minimum
number of words.

*/
void caml_do_print_reachable(long min) {
  Elf_info elf_info;
  int printed_glob_header = 0;
  int printed_dyn_header = 0;
  int printed_local_C_header = 0;
  struct caml__roots_block *lr;
  dyn_root_link *lnk;
  int i, j;
  value *glob;

  char *msg = elf_setup(&elf_info);
  if (strcmp(msg, "") != 0) {
    printf("Gc.print_reachable: ELF setup error: %s\n", msg);
    return;
  }

  for (i = 0; caml_globals[i] != 0; i++) {
    for (glob = caml_globals[i]; *glob != 0; glob++) {
      char *sym = get_elf_symbol_by_address(&elf_info, glob);
      int gnum = 0;
      for (j = 0; j < Wosize_val(*glob); j++, gnum++){
        ELF_Uint reachable =
            Long_val(caml_obj_reachable_words(Field (*glob, j)));
        if (reachable >= min) {
          if (!printed_glob_header) {
            printf("Globals:\n");
            printed_glob_header = 1;
          }
          printf("%10" ELF_INT_PRINTF_FORMAT "d field %3d %s\n",
              reachable, j, sym);
        }
      }
    }
  }

  /* Dynamic global roots */
  iter_list(caml_dyn_globals, lnk) {
    for (glob = (value *) lnk->data; *glob != 0; glob++) {
      for (j = 0; j < Wosize_val(*glob); j++){
        ELF_Uint reachable =
            Long_val(caml_obj_reachable_words(Field (*glob, j)));
        if (reachable >= min) {
          if (!printed_dyn_header) {
            printf("\nDynamic globals:\n");
            printed_dyn_header = 1;
          }
          printf("%10" ELF_INT_PRINTF_FORMAT "d field %3d %s\n",
              reachable, j, lnk->symname);
        }
      }
    }
  }

  /* Local C roots */
  for (lr = Caml_state->local_roots; lr != NULL; lr = lr->next) {
    for (i = 0; i < lr->ntables; i++) {
      for (j = 0; j < lr->nitems; j++) {
        value *root = &(lr->tables[i][j]);
        ELF_Uint reachable = Long_val(caml_obj_reachable_words((value)root));
        if (reachable >= min) {
          if (!printed_local_C_header) {
            printf("\nLocal C:\n");
            printed_local_C_header = 1;
          }
          printf("%10" ELF_INT_PRINTF_FORMAT "d item  %3d %"
              ELF_INT_PRINTF_FORMAT "x\n", reachable, j, (ELF_Uint)root);
        }
      }
    }
  }

  print_roots.min = min;
  print_roots.printed_header = 0;
  print_roots.item_num = 0;
  print_roots.header_string = "Global C";
  caml_scan_global_roots(root_report);

  print_roots.printed_header = 0;
  print_roots.item_num = 0;
  print_roots.header_string = "Finalized values";
  caml_final_do_roots (root_report);

  print_roots.printed_header = 0;
  print_roots.item_num = 0;
  print_roots.header_string = "Memprof";
  caml_memprof_do_roots (root_report);

  print_roots.printed_header = 0;
  print_roots.item_num = 0;
  print_roots.header_string = "Hook";
  if (caml_scan_roots_hook != NULL) (*caml_scan_roots_hook)(root_report);

  print_roots.printed_header = 0;
  do_stack_roots(min);

  if (elf_info.buf != NULL)
    free(elf_info.buf);
}

// returns rem_depth or -1 if not found
static int get_path(int rem_depth, int *pitem, value block, value sym) {
  int j;

  for (j = 0; j < Wosize_val(block); j++) {
    value sub_block = Field(block, j);
    if (sub_block == sym) {
      *pitem = j;
      return rem_depth;
    } else if (rem_depth > 1 && Is_block(sub_block)) {
      int rv = get_path(rem_depth-1, pitem+1, sub_block, sym);
      if (rv != -1) {
        *pitem = j;
        return rv;
      }
    }
  }

  return -1;
}

/* Finds the shortest "field path" for sym, which can be used to identify
the variable associated with global or dynamic heap root.  This is the main
code implementing Gc.field-path.

Note there can be multiple paths for symbols, such as those defined in
dynamically loaded files, hence all roots are searched up to max deep to
find the shortest field path
*/
int caml_do_field_path(value sym, int *items, int max) {
  int i, best_rem_depth = -1;
  int t_size = max*sizeof (int);
  int *t_items = malloc(t_size);
  value *glob;
  dyn_root_link *lnk;

  for (i = 0; caml_globals[i] != 0; i++) {
    for (glob = caml_globals[i]; *glob != 0; glob++) {
      int rem_depth = get_path(max, t_items, *glob, sym);
      if (rem_depth > best_rem_depth) {
        best_rem_depth = rem_depth;
        memcpy(items, t_items, t_size);
      }
    }
  }

  iter_list(caml_dyn_globals, lnk) {
    for (glob = (value *) lnk->data; *glob != 0; glob++) {
      int rem_depth = get_path(max, t_items, *glob, sym);
      if (rem_depth > best_rem_depth) {
        best_rem_depth = rem_depth;
        memcpy(items, t_items, t_size);
      }
    }
  }

  free(t_items);
  return best_rem_depth;
}
