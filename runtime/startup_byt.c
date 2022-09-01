/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*          Xavier Leroy and Damien Doligez, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 1996 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#define CAML_INTERNALS

/* Start-up code */

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include "caml/config.h"
#ifdef HAS_UNISTD
#include <unistd.h>
#endif
#ifdef _WIN32
#include <process.h>
#endif
#include "caml/alloc.h"
#include "caml/backtrace.h"
#include "caml/callback.h"
#include "caml/codefrag.h"
#include "caml/custom.h"
#include "caml/debugger.h"
#include "caml/domain_state.h"
#include "caml/dynlink.h"
#include "caml/runtime_events.h"
#include "caml/exec.h"
#include "caml/fail.h"
#include "caml/fiber.h"
#include "caml/fix_code.h"
#include "caml/gc_ctrl.h"
#include "caml/instrtrace.h"
#include "caml/interp.h"
#include "caml/intext.h"
#include "caml/io.h"
#include "caml/memory.h"
#include "caml/minor_gc.h"
#include "caml/misc.h"
#include "caml/mlvalues.h"
#include "caml/osdeps.h"
#include "caml/prims.h"
#include "caml/printexc.h"
#include "caml/reverse.h"
#include "caml/signals.h"
#include "caml/sys.h"
#include "caml/startup.h"
#include "caml/startup_aux.h"
#include "caml/version.h"

#include "build_config.h"

#ifndef O_BINARY
#define O_BINARY 0
#endif

#ifndef SEEK_END
#define SEEK_END 2
#endif

static char magicstr[EXEC_MAGIC_LENGTH+1];

/* Print the specified error message followed by an end-of-line and exit */
static void error(char *msg, ...)
{
  va_list ap;
  va_start(ap, msg);
  vfprintf (stderr, msg, ap);
  va_end(ap);
  fprintf(stderr, "\n");
  exit(127);
}

/* Read the trailer of a bytecode file */

static void fixup_endianness_trailer(uint32_t * p)
{
#ifndef ARCH_BIG_ENDIAN
  Reverse_32(p, p);
#endif
}

static int read_trailer(int fd, struct exec_trailer *trail)
{
  if (lseek(fd, (long) -TRAILER_SIZE, SEEK_END) == -1)
    return BAD_BYTECODE;
  if (read(fd, (char *) trail, TRAILER_SIZE) < TRAILER_SIZE)
    return BAD_BYTECODE;
  fixup_endianness_trailer(&trail->num_sections);
  memcpy(magicstr, trail->magic, EXEC_MAGIC_LENGTH);
  magicstr[EXEC_MAGIC_LENGTH] = 0;

  if (caml_params->print_magic) {
    printf("%s\n", magicstr);
    exit(0);
  }
  return
    (strncmp(trail->magic, EXEC_MAGIC, sizeof(trail->magic)) == 0)
      ? 0 : WRONG_MAGIC;
}

enum caml_byte_program_mode caml_byte_program_mode = STANDARD;

int caml_attempt_open(char_os **name, struct exec_trailer *trail,
                      int do_open_script)
{
  char_os * truename;
  int fd;
  int err;
  char buf [2], * u8;

  truename = caml_search_exe_in_path(*name);
  u8 = caml_stat_strdup_of_os(truename);
  caml_gc_message(0x100, "Opening bytecode executable %s\n", u8);
  caml_stat_free(u8);
  fd = open_os(truename, O_RDONLY | O_BINARY);
  if (fd == -1) {
    caml_stat_free(truename);
    caml_gc_message(0x100, "Cannot open file\n");
    if (errno == EMFILE)
      return NO_FDS;
    else
      return FILE_NOT_FOUND;
  }
  if (!do_open_script) {
    err = read (fd, buf, 2);
    if (err < 2 || (buf [0] == '#' && buf [1] == '!')) {
      close(fd);
      caml_stat_free(truename);
      caml_gc_message(0x100, "Rejected #! script\n");
      return BAD_BYTECODE;
    }
  }
  err = read_trailer(fd, trail);
  if (err != 0) {
    close(fd);
    caml_stat_free(truename);
    caml_gc_message(0x100, "Not a bytecode executable\n");
    return err;
  }
  *name = truename;
  return fd;
}

/* Read the section descriptors */

void caml_read_section_descriptors(int fd, struct exec_trailer *trail)
{
  int toc_size, i;

  toc_size = trail->num_sections * 8;
  trail->section = caml_stat_alloc(toc_size);
  lseek(fd, - (long) (TRAILER_SIZE + toc_size), SEEK_END);
  if (read(fd, (char *) trail->section, toc_size) != toc_size)
    caml_fatal_error("cannot read section table");
  /* Fixup endianness of lengths */
  for (i = 0; i < trail->num_sections; i++)
    fixup_endianness_trailer(&(trail->section[i].len));
}

/* Position fd at the beginning of the section having the given name.
   Return the length of the section data in bytes, or -1 if no section
   found with that name. */

int32_t caml_seek_optional_section(int fd, struct exec_trailer *trail,
                                   char *name)
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

int32_t caml_seek_section(int fd, struct exec_trailer *trail, char *name)
{
  int32_t len = caml_seek_optional_section(fd, trail, name);
  if (len == -1)
    caml_fatal_error("section `%s' is missing", name);
  return len;
}

/* Read and return the contents of the section having the given name.
   Add a terminating 0.  Return NULL if no such section. */

static char * read_section(int fd, struct exec_trailer *trail, char *name)
{
  int32_t len;
  char * data;

  len = caml_seek_optional_section(fd, trail, name);
  if (len == -1) return NULL;
  data = caml_stat_alloc(len + 1);
  if (read(fd, data, len) != len)
    caml_fatal_error("error reading section %s", name);
  data[len] = 0;
  return data;
}

#ifdef _WIN32

static char_os * read_section_to_os(int fd, struct exec_trailer *trail,
                                    char *name)
{
  int32_t len, wlen;
  char * data;
  wchar_t * wdata;

  len = caml_seek_optional_section(fd, trail, name);
  if (len == -1) return NULL;
  data = caml_stat_alloc(len + 1);
  if (read(fd, data, len) != len)
    caml_fatal_error("error reading section %s", name);
  data[len] = 0;
  wlen = caml_win32_multi_byte_to_wide_char(data, len, NULL, 0);
  wdata = caml_stat_alloc((wlen + 1)*sizeof(wchar_t));
  caml_win32_multi_byte_to_wide_char(data, len, wdata, wlen);
  wdata[wlen] = 0;
  caml_stat_free(data);
  return wdata;
}

#else

#define read_section_to_os read_section

#endif

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
      OCaml program.
  2-  In all other cases, we parse the command line as:
        (whatever) [options] bytecode args...
      and we strip "(whatever) [options]" from the command line.

*/

static void do_print_help(void)
{
  printf("%s\n",
    "Usage: ocamlrun [<options>] [--] <executable> [<command-line>]\n"
    "Options are:\n"
    "  -b  Set runtime parameter b (detailed exception backtraces)\n"
    "  -config  Print configuration values and exit\n"
    "  -I <dir>  Add <dir> to the list of DLL search directories\n"
    "  -m  Print the magic number of <executable> and exit\n"
    "  -M  Print the magic number expected by this runtime and exit\n"
    "  -p  Print the names of the primitives known to this runtime\n"
    "  -t  Trace the execution of the bytecode interpreter (specify multiple\n"
    "      times to increase verbosity)\n"
    "  -v  Set runtime parameter v=61 (GC event information)\n"
    "  -version  Print version string and exit\n"
    "  -vnum  Print short version number and exit\n"
    "  -help  Display this list of options\n"
    "  --help  Display this list of options");
}

/* Parse options on the command line */

static int parse_command_line(char_os **argv)
{
  int i, j, len, parsed;
  /* cast to make caml_params mutable; this assumes we are only called
     by one thread at startup */
  struct caml_params* params = (struct caml_params*)caml_params;

  for(i = 1; argv[i] != NULL && argv[i][0] == '-'; i++) {
    len = strlen_os(argv[i]);
    parsed = 1;
    if (len == 2) {
      /* Single-letter options, e.g. -v */
      switch(argv[i][1]) {
      case '-':
        return i + 1;
        break;
      case 't':
        params->trace_level += 1; /* ignored unless DEBUG mode */
        break;
      case 'v':
        params->verb_gc = 0x001+0x004+0x008+0x010+0x020;
        break;
      case 'p':
        for (j = 0; caml_names_of_builtin_cprim[j] != NULL; j++)
          printf("%s\n", caml_names_of_builtin_cprim[j]);
        exit(0);
        break;
      case 'b':
        caml_record_backtraces(1);
        break;
      case 'I':
        if (argv[i + 1] != NULL) {
          caml_ext_table_add(&caml_shared_libs_path, argv[i + 1]);
          i++;
        } else {
          error("option '-I' needs an argument.");
        }
        break;
      case 'm':
        params->print_magic = 1;
        break;
      case 'M':
        printf("%s\n", EXEC_MAGIC);
        exit(0);
        break;
      default:
        parsed = 0;
      }
    } else {
      /* Named options, e.g. -version */
      if (!strcmp_os(argv[i], T("-version"))) {
        printf("%s\n", "The OCaml runtime, version " OCAML_VERSION_STRING);
        exit(0);
      } else if (!strcmp_os(argv[i], T("-vnum"))) {
        printf("%s\n", OCAML_VERSION_STRING);
        exit(0);
      } else if (!strcmp_os(argv[i], T("-help")) ||
                 !strcmp_os(argv[i], T("--help"))) {
        do_print_help();
        exit(0);
      } else if (!strcmp_os(argv[i], T("-config"))) {
        params->print_config = 1;
      } else {
        parsed = 0;
      }
    }

    if (!parsed)
      error("unknown option %s", caml_stat_strdup_of_os(argv[i]));
  }

  return i;
}

/* Print the configuration of the runtime to stdout; memory allocated is not
   freed, since the runtime will terminate after calling this. */
static void do_print_config(void)
{
  int i;
  char_os * dir;

  /* Print the runtime configuration */
  printf("version: %s\n", OCAML_VERSION_STRING);
  printf("standard_library_default: %s\n",
         caml_stat_strdup_of_os(OCAML_STDLIB_DIR));
  printf("standard_library: %s\n",
         caml_stat_strdup_of_os(caml_get_stdlib_location()));
  printf("int_size: %d\n", 8 * (int)sizeof(value));
  printf("word_size: %d\n", 8 * (int)sizeof(value) - 1);
  printf("os_type: %s\n", OCAML_OS_TYPE);
  printf("host: %s\n", HOST);
  printf("flat_float_array: %s\n",
#ifdef FLAT_FLOAT_ARRAY
         "true");
#else
         "false");
#endif
  printf("supports_afl: %s\n",
#ifdef HAS_SYS_SHM_H
         "true");
#else
         "false");
#endif
  printf("windows_unicode: %s\n",
#if WINDOWS_UNICODE
         "true");
#else
         "false");
#endif
  printf("supports_shared_libraries: %s\n",
#ifdef SUPPORT_DYNAMIC_LINKING
         "true");
#else
         "false");
#endif
  printf("no_naked_pointers: true\n");
  printf("profinfo: %s\n"
         "profinfo_width: %d\n",
#ifdef WITH_PROFINFO
         "true", PROFINFO_WIDTH);
#else
         "false", 0);
#endif
  printf("exec_magic_number: %s\n", EXEC_MAGIC);

  /* Parse ld.conf and print the effective search path */
  puts("shared_libs_path:");
  caml_parse_ld_conf();
  for (i = 0; i < caml_shared_libs_path.size; i++) {
    dir = caml_shared_libs_path.contents[i];
    if (dir[0] == 0)
#ifdef _WIN32
      /* See caml_search_in_path in win32.c */
      continue;
#else
      dir = ".";
#endif
    printf("  %s\n", caml_stat_strdup_of_os(dir));
  }
}

#ifdef _WIN32
extern void caml_signal_thread(void * lpParam);
#endif

#ifdef _MSC_VER

/* PR 4887: avoid crash box of windows runtime on some system calls */
extern void caml_install_invalid_parameter_handler(void);

#endif

/* Main entry point when loading code from a file */

CAMLexport void caml_main(char_os **argv)
{
  int fd, pos;
  struct exec_trailer trail;
  struct channel * chan;
  value res;
  char * req_prims;
  char_os * shared_lib_path, * shared_libs;
  char_os * exe_name, * proc_self_exe;

  /* Initialize the domain */
  CAML_INIT_DOMAIN_STATE;

  /* Determine options */
  caml_parse_ocamlrunparam();

#ifdef DEBUG
  caml_gc_message (-1, "### OCaml runtime: debug mode ###\n");
#endif
  if (!caml_startup_aux(/* pooling */ caml_params->cleanup_on_exit))
    return;

  caml_init_codefrag();

  caml_init_locale();
#ifdef _MSC_VER
  caml_install_invalid_parameter_handler();
#endif
  caml_init_custom_operations();
  caml_init_os_params();
  caml_ext_table_init(&caml_shared_libs_path, 8);

  /* Determine position of bytecode file */
  pos = 0;

  /* First, try argv[0] (when ocamlrun is called by a bytecode program) */
  exe_name = argv[0];
  fd = caml_attempt_open(&exe_name, &trail, 0);

  /* Little grasshopper wonders why we do that at all, since
     "The current executable is ocamlrun itself, it's never a bytecode
     program".  Little grasshopper "ocamlc -custom" in mind should keep.
     With -custom, we have an executable that is ocamlrun itself
     concatenated with the bytecode.  So, if the attempt with argv[0]
     failed, it is worth trying again with executable_name. */
  if (fd < 0 && (proc_self_exe = caml_executable_name()) != NULL) {
    exe_name = proc_self_exe;
    fd = caml_attempt_open(&exe_name, &trail, 0);
  }

  if (fd < 0) {
    pos = parse_command_line(argv);
    if (caml_params->print_config) {
      do_print_config();
      exit(0);
    }
    if (argv[pos] == 0) {
      error("no bytecode file specified");
    }
    exe_name = argv[pos];
    fd = caml_attempt_open(&exe_name, &trail, 1);
    switch(fd) {
    case FILE_NOT_FOUND:
      error("cannot find file '%s'",
                       caml_stat_strdup_of_os(argv[pos]));
      break;
    case BAD_BYTECODE:
      error(
        "the file '%s' is not a bytecode executable file",
        caml_stat_strdup_of_os(exe_name));
      break;
    case WRONG_MAGIC:
      error(
        "the file '%s' has not the right magic number: "\
        "expected %s, got %s",
        caml_stat_strdup_of_os(exe_name),
        EXEC_MAGIC,
        magicstr);
      break;
    }
  }
  /* Read the table of contents (section descriptors) */
  caml_read_section_descriptors(fd, &trail);
  /* Initialize the abstract machine */
  caml_init_gc ();

  /* bring up runtime_events after we've initialised the gc */
  CAML_RUNTIME_EVENTS_INIT();

  Caml_state->external_raise = NULL;
  /* Setup signal handling */
  caml_init_signals();
  /* Initialize the interpreter */
  caml_interprete(NULL, 0);
  /* Initialize the debugger, if needed */
  caml_debugger_init();
  /* Load the code */
  caml_code_size = caml_seek_section(fd, &trail, "CODE");
  caml_load_code(fd, caml_code_size);
  caml_init_debug_info();
  /* Build the table of primitives */
  shared_lib_path = read_section_to_os(fd, &trail, "DLPT");
  shared_libs = read_section_to_os(fd, &trail, "DLLS");
  req_prims = read_section(fd, &trail, "PRIM");
  if (req_prims == NULL) caml_fatal_error("no PRIM section");
  caml_build_primitive_table(shared_lib_path, shared_libs, req_prims);
  caml_stat_free(shared_lib_path);
  caml_stat_free(shared_libs);
  caml_stat_free(req_prims);
  /* Load the globals */
  caml_seek_section(fd, &trail, "DATA");
  chan = caml_open_descriptor_in(fd);
  value global_data = caml_input_val(chan);
  caml_modify_generational_global_root(&caml_global_data, global_data);
  caml_close_channel(chan); /* this also closes fd */
  caml_stat_free(trail.section);
  /* Initialize system libraries */
  caml_sys_init(exe_name, argv + pos);
  /* Load debugging info, if b>=2 */
  caml_load_main_debug_info();
  /* ensure all globals are in major heap */
  caml_minor_collection();
#ifdef _WIN32
  /* Start a thread to handle signals */
  if (caml_secure_getenv(T("CAMLSIGPIPE")))
    _beginthread(caml_signal_thread, 4096, NULL);
#endif
  /* Execute the program */
  caml_debugger(PROGRAM_START, Val_unit);
  res = caml_interprete(caml_start_code, caml_code_size);
  if (Is_exception_result(res)) {
    value exn = Extract_exception(res);
    if (caml_debugger_in_use) {
      Caml_state->current_stack->sp = &exn; /* The debugger needs the
                                               exception value.*/
      caml_debugger(UNCAUGHT_EXC, Val_unit);
    }
    caml_fatal_uncaught_exception(exn);
  }
  caml_terminate_signals();
}

/* Main entry point when code is linked in as initialized data */

CAMLexport value caml_startup_code_exn(
           code_t code, asize_t code_size,
           char *data, asize_t data_size,
           char *section_table, asize_t section_table_size,
           int pooling,
           char_os **argv)
{
  char_os * exe_name;
  value res;

  /* Initialize the domain */
  CAML_INIT_DOMAIN_STATE;

  /* Determine options */
  caml_parse_ocamlrunparam();

#ifdef DEBUG
  caml_gc_message (-1, "### OCaml runtime: debug mode ###\n");
#endif
  if (caml_params->cleanup_on_exit)
    pooling = 1;
  if (!caml_startup_aux(pooling))
    return Val_unit;

  caml_init_codefrag();

  caml_init_locale();
#ifdef _MSC_VER
  caml_install_invalid_parameter_handler();
#endif
  caml_init_custom_operations();
  caml_init_os_params();

  /* Initialize the abstract machine */
  caml_init_gc ();

  /* runtime_events has to be brought up after the gc */
  CAML_RUNTIME_EVENTS_INIT();

  exe_name = caml_executable_name();
  if (exe_name == NULL) exe_name = caml_search_exe_in_path(argv[0]);

  Caml_state->external_raise = NULL;
  caml_sys_init(exe_name, argv);
  /* Load debugging info, if b>=2 */
  caml_load_main_debug_info();
  Caml_state->external_raise = NULL;
  /* Setup signal handling */
  caml_init_signals();
  /* Initialize the interpreter */
  caml_interprete(NULL, 0);
  /* Initialize the debugger, if needed */
  caml_debugger_init();
  /* Load the code */
  caml_start_code = code;
  caml_code_size = code_size;
  caml_init_code_fragments();
  caml_init_debug_info();
#ifdef THREADED_CODE
  caml_thread_code(caml_start_code, code_size);
#endif
  /* Use the builtin table of primitives */
  caml_build_primitive_table_builtin();
  /* Load the globals */
  caml_modify_generational_global_root
    (&caml_global_data, caml_input_value_from_block(data, data_size));
  caml_minor_collection(); /* ensure all globals are in major heap */
  /* Record the sections (for caml_get_section_table in meta.c) */
  caml_init_section_table(section_table, section_table_size);
  /* Initialize system libraries */
  caml_sys_init(exe_name, argv);
  /* Load debugging info, if b>=2 */
  caml_load_main_debug_info();
  /* Execute the program */
  caml_debugger(PROGRAM_START, Val_unit);
  res = caml_interprete(caml_start_code, caml_code_size);
  caml_terminate_signals();
  return res;
}

CAMLexport void caml_startup_code(
           code_t code, asize_t code_size,
           char *data, asize_t data_size,
           char *section_table, asize_t section_table_size,
           int pooling,
           char_os **argv)
{
  value res;

  res = caml_startup_code_exn(code, code_size, data, data_size,
                              section_table, section_table_size,
                              pooling, argv);
  if (Is_exception_result(res)) {
    value exn = Extract_exception(res);
    if (caml_debugger_in_use) {
      Caml_state->current_stack->sp = &exn; /* The debugger needs the
                                               exception value.*/
      caml_debugger(UNCAUGHT_EXC, Val_unit);
    }
    caml_fatal_uncaught_exception(exn);
  }
}
