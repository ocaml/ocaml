/*************************************************************************/
/*                                                                       */
/*                Objective Caml LablTk library                          */
/*                                                                       */
/*         Francois Rouaix, Francois Pessaux and Jun Furuse              */
/*               projet Cristal, INRIA Rocquencourt                      */
/*            Jacques Garrigue, Kyoto University RIMS                    */
/*                                                                       */
/*   Copyright 1999 Institut National de Recherche en Informatique et    */
/*   en Automatique and Kyoto University.  All rights reserved.          */
/*   This file is distributed under the terms of the GNU Library         */
/*   General Public License, with the special exception on linking       */
/*   described in file ../../../LICENSE.                                 */
/*                                                                       */
/*************************************************************************/

/* $Id$ */

#include <unistd.h>
#include <fcntl.h>  
#include <tcl.h>
#include <tk.h>
#include "gc.h"
#include "exec.h"
#include "sys.h"
#include "fail.h"
#include "io.h"
#include "mlvalues.h"
#include "memory.h"
#include "camltk.h"

#ifndef O_BINARY
#define O_BINARY 0
#endif


/* 
 * Dealing with signals: when a signal handler is defined in Caml,
 * the actual execution of the signal handler upon reception of the
 * signal is delayed until we are sure we are out of the GC.
 * If a signal occurs during the MainLoop, we would have to wait
 *  the next event for the handler to be invoked.
 * The following function will invoke a pending signal handler if any,
 * and we put in on a regular timer.
 */

#define SIGNAL_INTERVAL 300

int signal_events = 0; /* do we have a pending timer */

void invoke_pending_caml_signals (clientdata) 
     ClientData clientdata;
{
  signal_events = 0;
  enter_blocking_section(); /* triggers signal handling */
  /* Rearm timer */
  Tk_CreateTimerHandler(SIGNAL_INTERVAL, invoke_pending_caml_signals, NULL);
  signal_events = 1;
  leave_blocking_section();
}
/* The following is taken from byterun/startup.c */
header_t atom_table[256];
code_t start_code;
asize_t code_size;

static void init_atoms()
{
  int i;
  for(i = 0; i < 256; i++) atom_table[i] = Make_header(0, i, White);
}

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


/* Command for loading the bytecode file */
int CamlRunCmd(dummy, interp, argc, argv)
    ClientData dummy;                   /* Not used. */
    Tcl_Interp *interp;                 /* Current interpreter. */
    int argc;                           /* Number of arguments. */
    char **argv;                        /* Argument strings. */
{
  int fd;
  struct exec_trailer trail;
  struct longjmp_buffer raise_buf;
  struct channel * chan;

  if (argc < 2) {
        Tcl_AppendResult(interp, "wrong # args: should be \"",
                argv[0], " foo.cmo args\"", (char *) NULL);
        return TCL_ERROR;
  }
  fd = attempt_open(&argv[1], &trail, 1);

  switch(fd) {
  case FILE_NOT_FOUND:
    fatal_error_arg("Fatal error: cannot find file %s\n", argv[1]);
    break;
  case TRUNCATED_FILE:
  case BAD_MAGIC_NUM:
    fatal_error_arg(
                    "Fatal error: the file %s is not a bytecode executable file\n",
                    argv[1]);
    break;
  }

  if (sigsetjmp(raise_buf.buf, 1) == 0) {

    external_raise = &raise_buf;

    lseek(fd, - (long) (TRAILER_SIZE + trail.code_size + trail.data_size
                        + trail.symbol_size + trail.debug_size), 2);

    code_size = trail.code_size;
    start_code = (code_t) stat_alloc(code_size);
    if (read(fd, (char *) start_code, code_size) != code_size)
      fatal_error("Fatal error: truncated bytecode file.\n");

#ifdef ARCH_BIG_ENDIAN
    fixup_endianness(start_code, code_size);
#endif

    chan = open_descr(fd);
    global_data = input_value(chan);
    close_channel(chan);
    /* Ensure that the globals are in the major heap. */
    oldify(global_data, &global_data);

    sys_init(argv + 1);
    interprete(start_code, code_size);
    return TCL_OK;
  } else {
    Tcl_AppendResult(interp, "Caml program", argv[1], " raised exception \"",
                     String_val(Field(Field(exn_bucket, 0), 0)));
    return TCL_ERROR;
  }
}

int CamlInvokeCmd(dummy



/* Now the real Tk stuff */
Tk_Window cltk_mainWindow;

#define RCNAME ".camltkrc"
#define CAMLCB "camlcb"

/* Initialisation of the dynamically loaded module */
int Caml_Init(interp)
     Tcl_Interp *interp;
{
  cltclinterp = interp;
  /* Create the camlcallback command */
  Tcl_CreateCommand(cltclinterp,
                    CAMLCB, CamlCBCmd, 
                    (ClientData)NULL,(Tcl_CmdDeleteProc *)NULL);

  /* This is required by "unknown" and thus autoload */
  Tcl_SetVar(cltclinterp, "tcl_interactive", "0", TCL_GLOBAL_ONLY);
  /* Our hack for implementing break in callbacks */
  Tcl_SetVar(cltclinterp, "BreakBindingsSequence", "0", TCL_GLOBAL_ONLY);

  /* Load the traditional rc file */
  {
    char *home = getenv("HOME");
    if (home != NULL) {
      char *f = stat_alloc(strlen(home)+strlen(RCNAME)+2);
      f[0]='\0';
      strcat(f, home);
      strcat(f, "/");
      strcat(f, RCNAME);
      if (0 == access(f,R_OK)) 
        if (TCL_OK != Tcl_EvalFile(cltclinterp,f)) {
          stat_free(f);
          tk_error(Tcl_GetStringResult(cltclinterp));
        };
      stat_free(f);
    }
  }
  
  /* Initialisations from caml_main */
  {
    int verbose_init = 0,
        percent_free_init = Percent_free_def;
    long minor_heap_init = Minor_heap_def,
         heap_chunk_init = Heap_chunk_def;

    /* Machine-dependent initialization of the floating-point hardware
       so that it behaves as much as possible as specified in IEEE */
    init_ieee_floats();
    init_gc (minor_heap_init, heap_chunk_init, percent_free_init,
             verbose_init);
    init_stack();
    init_atoms();
  }
}
