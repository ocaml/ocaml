/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*         Xavier Leroy and Damien Doligez, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  Automatique.  Distributed only by permission.                      */
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
#include "alloc.h"
#include "debugger.h"
#include "exec.h"
#include "fail.h"
#include "fix_code.h"
#include "gc_ctrl.h"
#include "interp.h"
#include "intext.h"
#include "io.h"
#include "minor_gc.h"
#include "misc.h"
#include "mlvalues.h"
#include "stacks.h"
#include "sys.h"

#ifndef O_BINARY
#define O_BINARY 0
#endif

header_t atom_table[256];

/* Initialize the atom table */

static void init_atoms()
{
  int i;
  for(i = 0; i < 256; i++) atom_table[i] = Make_header(0, i, White);
}

/* Read the trailer of a bytecode file */

static unsigned long read_size(p)
     unsigned char * p;
{
  return ((unsigned long) p[0] << 24) + ((unsigned long) p[1] << 16) +
         ((unsigned long) p[2] << 8) + p[3];
}

#define FILE_NOT_FOUND (-1)
#define TRUNCATED_FILE (-2)
#define BAD_MAGIC_NUM (-3)

static int read_trailer(fd, trail)
     int fd;
     struct exec_trailer * trail;
{
  char buffer[TRAILER_SIZE];

  lseek(fd, (long) -TRAILER_SIZE, 2);
  if (read(fd, buffer, TRAILER_SIZE) < TRAILER_SIZE) return TRUNCATED_FILE;
  trail->code_size = read_size(buffer);
  trail->data_size = read_size(buffer+4);
  trail->symbol_size = read_size(buffer+8);
  trail->debug_size = read_size(buffer+12);
  if (strncmp(buffer + 16, EXEC_MAGIC, 12) == 0)
    return 0;
  else
    return BAD_MAGIC_NUM;
}

int attempt_open(name, trail, do_open_script)
     char ** name;
     struct exec_trailer * trail;
     int do_open_script;
{
  char * truename;
  int fd;
  int err;
  char buf [2];

  truename = searchpath(*name);
  if (truename == 0) truename = *name; else *name = truename;
  fd = open(truename, O_RDONLY | O_BINARY);
  if (fd == -1) return FILE_NOT_FOUND;
  if (!do_open_script){
    err = read (fd, buf, 2);
    if (err < 2) { close(fd); return TRUNCATED_FILE; }
    if (buf [0] == '#' && buf [1] == '!') { close(fd); return BAD_MAGIC_NUM; }
  }
  err = read_trailer(fd, trail);
  if (err != 0) { close(fd); return err; }
  return fd;
}

/* Invocation of camlrun: 4 cases.

   1.  runtime + bytecode
       user types:  camlrun [options] bytecode args...
       arguments:  camlrun [options] bytecode args...

   2.  bytecode script
       user types:  bytecode args...
   2a  (kernel 1) arguments:  camlrun ./bytecode args...
   2b  (kernel 2) arguments:  bytecode bytecode args...

   3.  concatenated runtime and bytecode
       user types:  composite args...
       arguments:  composite args...

Algorithm:
  1-  If argument 0 is a valid byte-code file that does not start with #!,
      then we are in case 3 and we pass the same command line to the
      Caml Light program.
  2-  In all other cases, we parse the command line as:
        (whatever) [options] bytecode args...
      and we strip "(whatever) [options]" from the command line.

*/

/* Configuration parameters flags */

static int verbose_init = 0, percent_free_init = Percent_free_def;
static long minor_heap_init = Minor_heap_def, heap_chunk_init = Heap_chunk_def;
extern int trace_flag;

/* Parse options on the command line */

static int parse_command_line(argv)
     char ** argv;
{
  int i;

  for(i = 1; argv[i] != NULL && argv[i][0] == '-'; i++) {
    switch(argv[i][1]) {
#ifdef DEBUG
    case 't':
      trace_flag = 1;
      break;
#endif
    case 'v':
      verbose_init = 1;
      break;
    default:
      fatal_error_arg("Unknown option %s.\n", argv[i]);
    }
  }
  return i;
}

/* Parse the CAMLRUNPARAM variable */
/* The option letter for each runtime option is the first letter of the
   last word of the ML name of the option (see [stdlib/gc.mli]). */

static void parse_camlrunparam()
{
  char *opt = getenv ("CAMLRUNPARAM");
  if (opt != NULL){
    while (*opt != '\0'){
      switch (*opt++){
      case 's': sscanf (opt, "=%ld", &minor_heap_init); break;
      case 'i': sscanf (opt, "=%ld", &heap_chunk_init); break;
      case 'o': sscanf (opt, "=%d", &percent_free_init); break;
      case 'v': sscanf (opt, "=%d", &verbose_init); break;
      }
    }
  }
}

extern void init_ieee_floats P((void));

/* Main entry point when loading code from a file */

void caml_main(argv)
     char ** argv;
{
  int fd;
  struct exec_trailer trail;
  int pos;
  struct longjmp_buffer raise_buf;

  /* Machine-dependent initialization of the floating-point hardware
     so that it behaves as much as possible as specified in IEEE */
  init_ieee_floats();
  /* Determine options and position of bytecode file */
#ifdef DEBUG
  verbose_init = 1;
#endif
  parse_camlrunparam();
  pos = 0;
  fd = attempt_open(&argv[0], &trail, 0);
  if (fd < 0) {
    pos = parse_command_line(argv);
    if (argv[pos] == 0)
      fatal_error("No bytecode file specified.\n");
    fd = attempt_open(&argv[pos], &trail, 1);
    switch(fd) {
    case FILE_NOT_FOUND:
      fatal_error_arg("Fatal error: cannot find file %s\n", argv[pos]);
      break;
    case TRUNCATED_FILE:
    case BAD_MAGIC_NUM:
      fatal_error_arg(
        "Fatal error: the file %s is not a bytecode executable file\n",
        argv[pos]);
      break;
    }
  }
  /* Set up a catch-all exception handler */
  if (sigsetjmp(raise_buf.buf, 1) == 0) {
    external_raise = &raise_buf;
    /* Initialize the abstract machine */
    init_gc (minor_heap_init, heap_chunk_init, percent_free_init,
	     verbose_init);
    init_stack();
    init_atoms();
    /* Initialize the interpreter */
    interprete(NULL, 0);
    /* Initialize the debugger, if needed */
    debugger_init();
    /* Load the code */
    lseek(fd, - (long) (TRAILER_SIZE + trail.code_size + trail.data_size
                        + trail.symbol_size + trail.debug_size), 2);
    load_code(fd, trail.code_size);
    /* Load the globals */
    { struct channel * chan;
      Push_roots(r, 1);
      chan = open_descr(fd);
      r[0] = (value) chan;
      global_data = input_value(chan);
      close_channel(chan);
      Pop_roots();
    }
    /* Ensure that the globals are in the major heap. */
    oldify(global_data, &global_data);
    /* Record the command-line arguments */
    sys_init(argv + pos);
    /* Execute the program */
    debugger(PROGRAM_START);
    interprete(start_code, trail.code_size);
  } else {
    debugger(UNCAUGHT_EXC);
    fatal_uncaught_exception(exn_bucket);
  }
}

/* Main entry point when code is linked in as initialized data */

void caml_startup_code(code, code_size, data, argv)
     code_t code;
     asize_t code_size;
     char * data;
     char ** argv;
{
  struct longjmp_buffer raise_buf;

  init_ieee_floats();
#ifdef DEBUG
  verbose_init = 1;
#endif
  parse_camlrunparam();
  /* Set up a catch-all exception handler */
  if (sigsetjmp(raise_buf.buf, 1) == 0) {
    external_raise = &raise_buf;
    /* Initialize the abstract machine */
    init_gc (minor_heap_init, heap_chunk_init, percent_free_init,
	     verbose_init);
    init_stack();
    init_atoms();
    /* Load the code */
    start_code = code;
    /* Load the globals */
    global_data = Field(input_value_from_string((value)data, Val_int(0)), 0);
    /* Ensure that the globals are in the major heap. */
    oldify(global_data, &global_data);
    /* Run the code */
    sys_init(argv);
    interprete(start_code, code_size);
  } else {
    fatal_uncaught_exception(exn_bucket);
  }
}
  
