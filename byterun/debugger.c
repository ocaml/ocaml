/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  Automatique.  Distributed only by permission.                      */
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

int debugger_in_use = 0;
unsigned long event_count;

#if !defined(HAS_SOCKETS) || defined(_WIN32)

void debugger_init()
{
}

void debugger(event)
     enum event_kind event;
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

static void open_connection()
{
  dbg_socket = socket(sock_domain, SOCK_STREAM, 0);
  if (dbg_socket == -1 ||
      connect(dbg_socket, &sock_addr.s_gen, sock_addr_len) == -1)
    fatal_error("cannot connect to debugger");
  dbg_in = open_descr(dbg_socket);
  if (!debugger_in_use) register_global_root((value *) &dbg_in);
  dbg_out = open_descr(dbg_socket);
  if (!debugger_in_use) register_global_root((value *) &dbg_out);
  if (!debugger_in_use) putword(dbg_out, -1); /* first connection */
  putword(dbg_out, getpid());
  flush(dbg_out);
}

static void close_connection()
{
  close_channel(dbg_in);
  close_channel(dbg_out);
  dbg_socket = -1;              /* was closed by close_channel */
}

void debugger_init()
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
        fatal_error_arg("Unknown debugging host %s\n", address);
      bcopy(host->h_addr, &sock_addr.s_inet.sin_addr, host->h_length);
    }
    sock_addr.s_inet.sin_port = htons(atoi(port));
    sock_addr_len = sizeof(sock_addr.s_inet);
  }
  open_connection();
  debugger_in_use = 1;
  trap_barrier = stack_high;
}

static value getval(chan)
     struct channel * chan;
{
  value res;
  if (really_getblock(chan, (char *) &res, sizeof(res)) == 0)
    raise_end_of_file(); /* Bad, but consistent with getword */
  return res;
}

static void putval(chan, val)
     struct channel * chan;
     value val;
{
  really_putblock(chan, (char *) &val, sizeof(val));
}

#define Pc(sp) ((code_t)(sp[0]))
#define Env(sp) (sp[1])
#define Locals(sp) (sp + 3)

void debugger(event)
     enum event_kind event;
{
  int frame_number;
  value * frame;
  long i, pos;
  mlsize_t size;
  value val;
  value * p;
  struct longjmp_buffer raise_buf, * saved_external_raise;

  if (dbg_socket == -1) return;  /* Not connected to a debugger. */

  /* Reset current frame */
  frame_number = 0;
  frame = extern_sp + 1;

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
  putword(dbg_out, event_count);
  putword(dbg_out, stack_high - frame);
  putword(dbg_out, (Pc(frame) - start_code) * sizeof(opcode_t));
  flush(dbg_out);

 command_loop:
  
  /* Read and execute the commands sent by the debugger */
  while(1) {
    switch(getch(dbg_in)) {
    case REQ_SET_EVENT:
      pos = getword(dbg_in);
      Assert(pos >= 0 && pos < code_size);
      set_instruction(start_code + pos / sizeof(opcode_t), EVENT);
      break;
    case REQ_SET_BREAKPOINT:
      pos = getword(dbg_in);
      Assert(pos >= 0 && pos < code_size);
      set_instruction(start_code + pos / sizeof(opcode_t), BREAK);
      break;
    case REQ_RESET_INSTR:
      pos = getword(dbg_in);
      Assert(pos >= 0 && pos < code_size);
      pos = pos / sizeof(opcode_t);
      set_instruction(start_code + pos, saved_code[pos]);
      break;
    case REQ_CHECKPOINT:
      i = fork();
      if (i == 0) {
	close_connection();	/* Close parent connection. */
	open_connection();      /* Open new connection with debugger */
      } else {
	putword(dbg_out, i);
	flush(dbg_out);
      }
      break;
    case REQ_GO:
      event_count = getword(dbg_in);
      return;
    case REQ_STOP:
      exit(0);
      break;
    case REQ_WAIT:
      wait(NULL);
      break;
    case REQ_INITIAL_FRAME:
      frame = extern_sp + 1;
      /* Fall through */
    case REQ_GET_FRAME:
      putword(dbg_out, stack_high - frame);
      putword(dbg_out, (Pc(frame) - start_code) * sizeof(opcode_t));
      flush(dbg_out);
      break;
    case REQ_SET_FRAME:
      i = getword(dbg_in);
      frame = stack_high - i;
      break;
    case REQ_UP_FRAME:
      i = getword(dbg_in);
      if (frame + i + 3 >= stack_high) {
        putword(dbg_out, -1);
      } else {
        frame += i + 3;
        putword(dbg_out, stack_high - frame);
        putword(dbg_out, (Pc(frame) - start_code) * sizeof(opcode_t));
      }
      flush(dbg_out);
      break;
    case REQ_SET_TRAP_BARRIER:
      i = getword(dbg_in);
      trap_barrier = stack_high - i;
      break;
    case REQ_GET_LOCAL:
      i = getword(dbg_in);
      putval(dbg_out, Locals(frame)[i]);
      flush(dbg_out);
      break;
    case REQ_GET_ENVIRONMENT:
      i = getword(dbg_in);
      putval(dbg_out, Field(Env(frame), i));
      flush(dbg_out);
      break;
    case REQ_GET_GLOBAL:
      i = getword(dbg_in);
      putval(dbg_out, Field(global_data, i));
      flush(dbg_out);
      break;
    case REQ_GET_ACCU:
      putval(dbg_out, *extern_sp);
      flush(dbg_out);
      break;
    case REQ_GET_HEADER:
      val = getval(dbg_in);
      putword(dbg_out, Hd_val(val));
      flush(dbg_out);
      break;
    case REQ_GET_FIELD:
      val = getval(dbg_in);
      i = getword(dbg_in);
      putval(dbg_out, Field(val, i));
      flush(dbg_out);
      break;
    case REQ_MARSHAL_OBJ:
      val = getval(dbg_in);
      /* Catch exceptions raised by output_value */
      saved_external_raise = external_raise;
      if (sigsetjmp(raise_buf.buf, 1) == 0) {
        external_raise = &raise_buf;
        output_value(dbg_out, val);
      } else {
        /* Send wrong magic number, will cause input_value to fail */
        really_putblock(dbg_out, "\000\000\000\000", 4);
      }
      external_raise = saved_external_raise;
      flush(dbg_out);
      break;
    case REQ_GET_CLOSURE_CODE:
      val = getval(dbg_in);
      putword(dbg_out, (Code_val(val) - start_code) * sizeof(opcode_t));
      flush(dbg_out);
      break;
    }
  }
}

#endif
