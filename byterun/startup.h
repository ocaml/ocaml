#ifndef _startup_
#define _startup_

#include "misc.h"
#include "exec.h"

CAMLextern void caml_main(char **argv);
CAMLextern void caml_startup_code(code_t code, asize_t code_size,
                                  char *data, char **argv);

enum { FILE_NOT_FOUND = -1, BAD_BYTECODE  = -2 };

extern int attempt_open(char **name, struct exec_trailer *trail,
                        int do_open_script);
extern void read_section_descriptors(int fd, struct exec_trailer *trail);
extern int32 seek_optional_section(int fd, struct exec_trailer *trail,
                                   char *name);
extern int32 seek_section(int fd, struct exec_trailer *trail, char *name);


#endif
