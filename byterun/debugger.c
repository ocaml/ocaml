/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

/* Interface with the debugger */

#include <string.h>

#include "config.h"
#include "debugger.h"
#include "fail.h"
#include "fix_code.h"
#include "instruct.h"
#include "intext.h"
#include "io.h"
#include "misc.h"
#include "mlvalues.h"
#include "stacks.h"
#include "sys.h"

int caml_debugger_in_use = 0;
unsigned long caml_event_count;

#if !defined(HAS_SOCKETS) || defined(_WIN32)

void caml_debugger_init(void)
{
}

void caml_debugger(enum event_kind event)
{
}

#else

#ifdef HAS_UNISTD
#include <unistd.h>
#endif
#include <sys/types.h>
#include <sys/wait.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>

static int sock_domain;         /* Socket domain for the debugger */
static union {                  /* Socket address for the debugger */
  struct sockaddr s_gen;
  struct sockaddr_un s_unix;
  struct sockaddr_in s_inet;
} sock_addr;
static int sock_addr_len;       /* Length of sock_addr */

static int dbg_socket = -1;     /* The socket connected to the debugger */
static struct channel * dbg_in; /* Input channel on the socket */
static struct channel * dbg_out;/* Output channel on the socket */

static void open_connection(void)
{
  dbg_socket = socket(sock_domain, SOCK_STREAM, 0);
  if (dbg_socket == -1 ||
      connect(dbg_socket, &sock_addr.s_gen, sock_addr_len) == -1)
    caml_fatal_error("cannot connect to debugger");
  dbg_in = caml_open_descriptor_in(dbg_socket);
  dbg_out = caml_open_descriptor_out(dbg_socket);
  if (!caml_debugger_in_use) caml_putword(dbg_out, -1); /* first connection */
  caml_putword(dbg_out, getpid());
  caml_flush(dbg_out);
}

static void close_connection(void)
{
  caml_close_channel(dbg_in);
  caml_close_channel(dbg_out);
  dbg_socket = -1;              /* was closed by caml_close_channel */
}

void caml_debugger_init(void)
{
  char * address;
  char * port, * p;
  struct hostent * host;
  int n;

  address = getenv("CAML_DEBUG_SOCKET");
  if (address == NULL) return;

  /* Parse the address */
  port = NULL;
  for (p = address; *p != 0; p++) {
    if (*p == ':') { *p = 0; port = p+1; break; }
  }
  if (port == NULL) {
    /* Unix domain */
    sock_domain = PF_UNIX;
    sock_addr.s_unix.sun_family = AF_UNIX;
    strncpy(sock_addr.s_unix.sun_path, address,
            sizeof(sock_addr.s_unix.sun_path));
    sock_addr_len = 
      ((char *)&(sock_addr.s_unix.sun_path) - (char *)&(sock_addr.s_unix))
        + strlen(address);
  } else {
    /* Internet domain */
    sock_domain = PF_INET;
    for (p = (char *) &sock_addr.s_inet, n = sizeof(sock_addr.s_inet);
         n > 0; n--) *p++ = 0;
    sock_addr.s_inet.sin_family = AF_INET;
    sock_addr.s_inet.sin_addr.s_addr = inet_addr(address);
    if (sock_addr.s_inet.sin_addr.s_addr == -1) {
      host = gethostbyname(address);
      if (host == NULL)
        caml_fatal_error_arg("Unknown debugging host %s\n", address);
      memmove(&sock_addr.s_inet.sin_addr, host->h_addr, host->h_length);
    }
    sock_addr.s_inet.sin_port = htons(atoi(port));
    sock_addr_len = sizeof(sock_addr.s_inet);
  }
  open_connection();
  caml_debugger_in_use = 1;
  caml_trap_barrier = caml_stack_high;
}

static value getval(struct channel *chan)
{
  value res;
  if (caml_really_getblock(chan, (char *) &res, sizeof(res)) == 0)
    caml_raise_end_of_file(); /* Bad, but consistent with caml_getword */
  return res;
}

static void putval(struct channel *chan, value val)
{
  caml_really_putblock(chan, (char *) &val, sizeof(val));
}

static void safe_output_value(struct channel *chan, value val)
{
  struct longjmp_buffer raise_buf, * saved_external_raise;

  /* Catch exceptions raised by [caml_output_val] */
  saved_external_raise = caml_external_raise;
  if (sigsetjmp(raise_buf.buf, 0) == 0) {
    caml_external_raise = &raise_buf;
    caml_output_val(chan, val, Val_unit);
  } else {
    /* Send wrong magic number, will cause [caml_input_value] to fail */
    caml_really_putblock(chan, "\000\000\000\000", 4);
  }
  caml_external_raise = saved_external_raise;
}

#define Pc(sp) ((code_t)((sp)[0]))
#define Env(sp) ((sp)[1])
#define Extra_args(sp) (Long_val(((sp)[2])))
#define Locals(sp) ((sp) + 3)

void caml_debugger(enum event_kind event)
{
  int frame_number;
  value * frame;
  long i, pos;
  value val;

  if (dbg_socket == -1) return;  /* Not connected to a debugger. */

  /* Reset current frame */
  frame_number = 0;
  frame = caml_extern_sp + 1;

  /* Report the event to the debugger */
  switch(event) {
  case PROGRAM_START:           /* Nothing to report */
    goto command_loop;
  case EVENT_COUNT:
    putch(dbg_out, REP_EVENT);
    break;
  case BREAKPOINT:
    putch(dbg_out, REP_BREAKPOINT);
    break;
  case PROGRAM_EXIT:
    putch(dbg_out, REP_EXITED);
    break;
  case TRAP_BARRIER:
    putch(dbg_out, REP_TRAP);
    break;
  case UNCAUGHT_EXC:
    putch(dbg_out, REP_UNCAUGHT_EXC);
    break;
  }
  caml_putword(dbg_out, caml_event_count);
  if (event == EVENT_COUNT || event == BREAKPOINT) {
    caml_putword(dbg_out, caml_stack_high - frame);
    caml_putword(dbg_out, (Pc(frame) - caml_start_code) * sizeof(opcode_t));
  } else {
    /* No PC and no stack frame associated with other events */
    caml_putword(dbg_out, 0);
    caml_putword(dbg_out, 0);
  }
  caml_flush(dbg_out);

 command_loop:
  
  /* Read and execute the commands sent by the debugger */
  while(1) {
    switch(getch(dbg_in)) {
    case REQ_SET_EVENT:
      pos = caml_getword(dbg_in);
      Assert (pos >= 0);
      Assert (pos < caml_code_size);
      caml_set_instruction(caml_start_code + pos / sizeof(opcode_t), EVENT);
      break;
    case REQ_SET_BREAKPOINT:
      pos = caml_getword(dbg_in);
      Assert (pos >= 0);
      Assert (pos < caml_code_size);
      caml_set_instruction(caml_start_code + pos / sizeof(opcode_t), BREAK);
      break;
    case REQ_RESET_INSTR:
      pos = caml_getword(dbg_in);
      Assert (pos >= 0);
      Assert (pos < caml_code_size);
      pos = pos / sizeof(opcode_t);
      caml_set_instruction(caml_start_code + pos, caml_saved_code[pos]);
      break;
    case REQ_CHECKPOINT:
      i = fork();
      if (i == 0) {
        close_connection();     /* Close parent connection. */
        open_connection();      /* Open new connection with debugger */
      } else {
        caml_putword(dbg_out, i);
        caml_flush(dbg_out);
      }
      break;
    case REQ_GO:
      caml_event_count = caml_getword(dbg_in);
      return;
    case REQ_STOP:
      exit(0);
      break;
    case REQ_WAIT:
      wait(NULL);
      break;
    case REQ_INITIAL_FRAME:
      frame = caml_extern_sp + 1;
      /* Fall through */
    case REQ_GET_FRAME:
      caml_putword(dbg_out, caml_stack_high - frame);
      if (frame < caml_stack_high){
        caml_putword(dbg_out, (Pc(frame) - caml_start_code) * sizeof(opcode_t));
      }else{
        caml_putword (dbg_out, 0);
      }
      caml_flush(dbg_out);
      break;
    case REQ_SET_FRAME:
      i = caml_getword(dbg_in);
      frame = caml_stack_high - i;
      break;
    case REQ_UP_FRAME:
      i = caml_getword(dbg_in);
      if (frame + Extra_args(frame) + i + 3 >= caml_stack_high) {
        caml_putword(dbg_out, -1);
      } else {
        frame += Extra_args(frame) + i + 3;
        caml_putword(dbg_out, caml_stack_high - frame);
        caml_putword(dbg_out, (Pc(frame) - caml_start_code) * sizeof(opcode_t));
      }
      caml_flush(dbg_out);
      break;
    case REQ_SET_TRAP_BARRIER:
      i = caml_getword(dbg_in);
      caml_trap_barrier = caml_stack_high - i;
      break;
    case REQ_GET_LOCAL:
      i = caml_getword(dbg_in);
      putval(dbg_out, Locals(frame)[i]);
      caml_flush(dbg_out);
      break;
    case REQ_GET_ENVIRONMENT:
      i = caml_getword(dbg_in);
      putval(dbg_out, Field(Env(frame), i));
      caml_flush(dbg_out);
      break;
    case REQ_GET_GLOBAL:
      i = caml_getword(dbg_in);
      putval(dbg_out, Field(caml_global_data, i));
      caml_flush(dbg_out);
      break;
    case REQ_GET_ACCU:
      putval(dbg_out, *caml_extern_sp);
      caml_flush(dbg_out);
      break;
    case REQ_GET_HEADER:
      val = getval(dbg_in);
      caml_putword(dbg_out, Hd_val(val));
      caml_flush(dbg_out);
      break;
    case REQ_GET_FIELD:
      val = getval(dbg_in);
      i = caml_getword(dbg_in);
      if (Tag_val(val) != Double_array_tag) {
        putch(dbg_out, 0);
        putval(dbg_out, Field(val, i));
      } else {
        double d = Double_field(val, i);
        putch(dbg_out, 1);
        caml_really_putblock(dbg_out, (char *) &d, 8);
      }
      caml_flush(dbg_out);
      break;
    case REQ_MARSHAL_OBJ:
      val = getval(dbg_in);
      safe_output_value(dbg_out, val);
      caml_flush(dbg_out);
      break;
    case REQ_GET_CLOSURE_CODE:
      val = getval(dbg_in);
      caml_putword(dbg_out, (Code_val(val)-caml_start_code) * sizeof(opcode_t));
      caml_flush(dbg_out);
      break;
    }
  }
}

#endif
