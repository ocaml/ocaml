/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*         Xavier Leroy and Damien Doligez, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

/* Start-up code */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include "config.h"
#ifdef HAS_UNISTD
#include <unistd.h>
#endif
#ifdef _WIN32
#include <process.h>
#endif
#include "alloc.h"
#include "backtrace.h"
#include "callback.h"
#include "custom.h"
#include "debugger.h"
#include "dynlink.h"
#include "exec.h"
#include "fail.h"
#include "fix_code.h"
#include "gc_ctrl.h"
#include "instrtrace.h"
#include "interp.h"
#include "intext.h"
#include "io.h"
#include "memory.h"
#include "minor_gc.h"
#include "misc.h"
#include "mlvalues.h"
#include "osdeps.h"
#include "prims.h"
#include "printexc.h"
#include "reverse.h"
#include "signals.h"
#include "stacks.h"
#include "sys.h"
#include "startup.h"

#ifndef O_BINARY
#define O_BINARY 0
#endif

#ifndef SEEK_END
#define SEEK_END 2
#endif

extern int parser_trace;

CAMLexport header_t atom_table[256];

/* Initialize the atom table */

static void init_atoms(void)
{
  int i;
  for(i = 0; i < 256; i++) atom_table[i] = Make_header(0, i, Caml_white);
}

/* Read the trailer of a bytecode file */

static void fixup_endianness_trailer(uint32 * p)
{
#ifndef ARCH_BIG_ENDIAN
  Reverse_32(p, p);
#endif
}

static int read_trailer(int fd, struct exec_trailer *trail)
{
  lseek(fd, (long) -TRAILER_SIZE, SEEK_END);
  if (read(fd, (char *) trail, TRAILER_SIZE) < TRAILER_SIZE)
    return BAD_BYTECODE;
  fixup_endianness_trailer(&trail->num_sections);
  if (strncmp(trail->magic, EXEC_MAGIC, 12) == 0)
    return 0;
  else
    return BAD_BYTECODE;
}

int attempt_open(char **name, struct exec_trailer *trail,
                 int do_open_script)
{
  char * truename;
  int fd;
  int err;
  char buf [2];

  truename = search_exe_in_path(*name);
  *name = truename;
  caml_gc_message(0x100, "Opening bytecode executable %s\n",
                  (unsigned long) truename);
  fd = open(truename, O_RDONLY | O_BINARY);
  if (fd == -1) {
    caml_gc_message(0x100, "Cannot open file\n", 0);
    return FILE_NOT_FOUND;
  }
  if (!do_open_script) {
    err = read (fd, buf, 2);
    if (err < 2 || (buf [0] == '#' && buf [1] == '!')) {
      close(fd);
      caml_gc_message(0x100, "Rejected #! script\n", 0);
      return BAD_BYTECODE;
    }
  }
  err = read_trailer(fd, trail);
  if (err != 0) {
    close(fd);
    caml_gc_message(0x100, "Not a bytecode executable\n", 0);
    return err;
  }
  return fd;
}

/* Read the section descriptors */

void read_section_descriptors(int fd, struct exec_trailer *trail)
{
  int toc_size, i;

  toc_size = trail->num_sections * 8;
  trail->section = stat_alloc(toc_size);
  lseek(fd, - (long) (TRAILER_SIZE + toc_size), SEEK_END);
  if (read(fd, (char *) trail->section, toc_size) != toc_size)
    caml_fatal_error("Fatal error: cannot read section table\n");
  /* Fixup endianness of lengths */
  for (i = 0; i < trail->num_sections; i++)
    fixup_endianness_trailer(&(trail->section[i].len));
}

/* Position fd at the beginning of the section having the given name.
   Return the length of the section data in bytes, or -1 if no section
   found with that name. */

int32 seek_optional_section(int fd, struct exec_trailer *trail, char *name)
{
  long ofs;
  int i;

  ofs = TRAILER_SIZE + trail->num_sections * 8;
  for (i = trail->num_sections - 1; i >= 0; i--) {
    ofs += trail->section[i].len;
    if (strncmp(trail->section[i].name, name, 4) == 0) {
      lseek(fd, -ofs, SEEK_END);
      return trail->section[i].len;
    }
  }
  return -1;
}

/* Position fd at the beginning of the section having the given name.
   Return the length of the section data in bytes. */

int32 seek_section(int fd, struct exec_trailer *trail, char *name)
{
  int32 len = seek_optional_section(fd, trail, name);
  if (len == -1) 
    caml_fatal_error_arg("Fatal_error: section `%s' is missing\n", name);
  return len;
}

/* Read and return the contents of the section having the given name.
   Add a terminating 0.  Return NULL if no such section. */

static char * read_section(int fd, struct exec_trailer *trail, char *name)
{
  int32 len;
  char * data;

  len = seek_optional_section(fd, trail, name);
  if (len == -1) return NULL;
  data = stat_alloc(len + 1);
  if (read(fd, data, len) != len)
    caml_fatal_error_arg("Fatal error: error reading section %s\n", name);
  data[len] = 0;
  return data;
}

/* Invocation of ocamlrun: 4 cases.

   1.  runtime + bytecode
       user types:  ocamlrun [options] bytecode args...
       arguments:  ocamlrun [options] bytecode args...

   2.  bytecode script
       user types:  bytecode args...
   2a  (kernel 1) arguments:  ocamlrun ./bytecode args...
   2b  (kernel 2) arguments:  bytecode bytecode args...

   3.  concatenated runtime and bytecode
       user types:  composite args...
       arguments:  composite args...

Algorithm:
  1-  If argument 0 is a valid byte-code file that does not start with #!,
      then we are in case 3 and we pass the same command line to the
      Objective Caml program.
  2-  In all other cases, we parse the command line as:
        (whatever) [options] bytecode args...
      and we strip "(whatever) [options]" from the command line.

*/

/* Configuration parameters and flags */

static unsigned long percent_free_init = Percent_free_def;
static unsigned long max_percent_free_init = Max_percent_free_def;
static unsigned long minor_heap_init = Minor_heap_def;
static unsigned long heap_chunk_init = Heap_chunk_def;
static unsigned long heap_size_init = Init_heap_def;
static unsigned long max_stack_init = Max_stack_def;

/* Parse options on the command line */

static int parse_command_line(char **argv)
{
  int i, j;

  for(i = 1; argv[i] != NULL && argv[i][0] == '-'; i++) {
    switch(argv[i][1]) {
#ifdef DEBUG
    case 't':
      trace_flag = 1;
      break;
#endif
    case 'v':
      caml_verb_gc = 0x001+0x004+0x008+0x010+0x020;
      break;
    case 'p':
      for (j = 0; names_of_builtin_cprim[j] != NULL; j++)
        printf("%s\n", names_of_builtin_cprim[j]);
      exit(0);
      break;
    case 'b':
      init_backtrace();
      break;
    case 'I':
      if (argv[i + 1] != NULL) {
        caml_ext_table_add(&shared_libs_path, argv[i + 1]);
        i++;
      }
      break;
    default:
      caml_fatal_error_arg("Unknown option %s.\n", argv[i]);
    }
  }
  return i;
}

/* Parse the CAMLRUNPARAM variable */
/* The option letter for each runtime option is the first letter of the
   last word of the ML name of the option (see [stdlib/gc.mli]).
   Except for l (maximum stack size) and h (initial heap size).
*/

/* If you change these functions, see also their copy in asmrun/startup.c */

static void scanmult (char *opt, long unsigned int *var)
{
  char mult = ' ';
  sscanf (opt, "=%lu%c", var, &mult);
  sscanf (opt, "=0x%lx%c", var, &mult);
  if (mult == 'k') *var = *var * 1024;
  if (mult == 'M') *var = *var * 1024 * 1024;
  if (mult == 'G') *var = *var * 1024 * 1024 * 1024;
}

static void parse_camlrunparam(void)
{
  char *opt = getenv ("OCAMLRUNPARAM");

  if (opt == NULL) opt = getenv ("CAMLRUNPARAM");

  if (opt != NULL){
    while (*opt != '\0'){
      switch (*opt++){
      case 's': scanmult (opt, &minor_heap_init); break;
      case 'i': scanmult (opt, &heap_chunk_init); break;
      case 'h': scanmult (opt, &heap_size_init); break;
      case 'l': scanmult (opt, &max_stack_init); break;
      case 'o': scanmult (opt, &percent_free_init); break;
      case 'O': scanmult (opt, &max_percent_free_init); break;
      case 'v': scanmult (opt, &caml_verb_gc); break;
      case 'b': init_backtrace(); break;
      case 'p': parser_trace = 1; break;
      }
    }
  }
}

extern void init_ieee_floats (void);

#ifdef _WIN32
extern void caml_signal_thread(void * lpParam);
#endif

/* Main entry point when loading code from a file */

CAMLexport void caml_main(char **argv)
{
  int fd, pos;
  struct exec_trailer trail;
  struct channel * chan;
  value res;
  char * shared_lib_path, * shared_libs, * req_prims;
  char * exe_name;
#ifdef __linux__
  static char proc_self_exe[256];
#endif

  /* Machine-dependent initialization of the floating-point hardware
     so that it behaves as much as possible as specified in IEEE */
  init_ieee_floats();
  init_custom_operations();
  caml_ext_table_init(&shared_libs_path, 8);
  external_raise = NULL;
  /* Determine options and position of bytecode file */
#ifdef DEBUG
  caml_verb_gc = 63;
#endif
  parse_camlrunparam();
  pos = 0;
  exe_name = argv[0];
#ifdef __linux__
  if (executable_name(proc_self_exe, sizeof(proc_self_exe)) == 0)
    exe_name = proc_self_exe;
#endif
  fd = attempt_open(&exe_name, &trail, 0);
  if (fd < 0) {
    pos = parse_command_line(argv);
    if (argv[pos] == 0)
      caml_fatal_error("No bytecode file specified.\n");
    exe_name = argv[pos];
    fd = attempt_open(&exe_name, &trail, 1);
    switch(fd) {
    case FILE_NOT_FOUND:
      caml_fatal_error_arg("Fatal error: cannot find file %s\n", argv[pos]);
      break;
    case BAD_BYTECODE:
      caml_fatal_error_arg(
        "Fatal error: the file %s is not a bytecode executable file\n",
        argv[pos]);
      break;
    }
  }
  /* Read the table of contents (section descriptors) */
  read_section_descriptors(fd, &trail);
  /* Initialize the abstract machine */
  init_gc (minor_heap_init, heap_size_init, heap_chunk_init,
           percent_free_init, max_percent_free_init);
  init_stack (max_stack_init);
  init_atoms();
  /* Initialize the interpreter */
  interprete(NULL, 0);
  /* Initialize the debugger, if needed */
  debugger_init();
  /* Load the code */
  code_size = seek_section(fd, &trail, "CODE");
  load_code(fd, code_size);
  /* Build the table of primitives */
  shared_lib_path = read_section(fd, &trail, "DLPT");
  shared_libs = read_section(fd, &trail, "DLLS");
  req_prims = read_section(fd, &trail, "PRIM");
  if (req_prims == NULL) caml_fatal_error("Fatal error: no PRIM section\n");
  build_primitive_table(shared_lib_path, shared_libs, req_prims);
  stat_free(shared_lib_path);
  stat_free(shared_libs);
  stat_free(req_prims);
  /* Load the globals */
  seek_section(fd, &trail, "DATA");
  chan = caml_open_descriptor_in(fd);
  global_data = input_val(chan);
  caml_close_channel(chan); /* this also closes fd */
  stat_free(trail.section);
  /* Ensure that the globals are in the major heap. */
  oldify_one (global_data, &global_data);
  oldify_mopup ();
  /* Initialize system libraries */
  init_exceptions();
  caml_sys_init(exe_name, argv + pos);
#ifdef _WIN32
  /* Start a thread to handle signals */
  if (getenv("CAMLSIGPIPE"))
    _beginthread(caml_signal_thread, 4096, NULL);
#endif
  /* Execute the program */
  debugger(PROGRAM_START);
  res = interprete(start_code, code_size);
  if (Is_exception_result(res)) {
    exn_bucket = Extract_exception(res);
    if (debugger_in_use) {
      extern_sp = &exn_bucket; /* The debugger needs the exception value. */
      debugger(UNCAUGHT_EXC);
    }
    fatal_uncaught_exception(exn_bucket);
  }
}

/* Main entry point when code is linked in as initialized data */

CAMLexport void caml_startup_code(code_t code, asize_t code_size,
                                  char *data, char **argv)
{
  value res;

  init_ieee_floats();
  init_custom_operations();
#ifdef DEBUG
  caml_verb_gc = 63;
#endif
  parse_camlrunparam();
  external_raise = NULL;
  /* Initialize the abstract machine */
  init_gc (minor_heap_init, heap_size_init, heap_chunk_init,
           percent_free_init, max_percent_free_init);
  init_stack (max_stack_init);
  init_atoms();
  /* Initialize the interpreter */
  interprete(NULL, 0);
  /* Load the code */
  start_code = code;
#ifdef THREADED_CODE
  thread_code(start_code, code_size);
#endif
  /* Use the builtin table of primitives */
  prim_table.size = prim_table.capacity = -1;
  prim_table.contents = (void **) builtin_cprim;
  /* Load the globals */
  global_data = input_val_from_string((value)data, 0);
  /* Ensure that the globals are in the major heap. */
  oldify_one (global_data, &global_data);
  oldify_mopup ();
  /* Run the code */
  init_exceptions();
  caml_sys_init("", argv);
  res = interprete(start_code, code_size);
  if (Is_exception_result(res))
    fatal_uncaught_exception(Extract_exception(res));
}

