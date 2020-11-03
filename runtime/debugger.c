/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           */
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

/* Interface with the byte-code debugger */

#ifdef _WIN32
#include <io.h>
#endif /* _WIN32 */

#include <string.h>

#include "caml/alloc.h"
#include "caml/codefrag.h"
#include "caml/config.h"
#include "caml/debugger.h"
#include "caml/misc.h"
#include "caml/osdeps.h"
#include "caml/skiplist.h"

int caml_debugger_in_use = 0;
uintnat caml_event_count;
int caml_debugger_fork_mode = 1; /* parent by default */

#if !defined(HAS_SOCKETS) || defined(NATIVE_CODE)

void caml_debugger_init(void)
{
}

void caml_debugger(enum event_kind event, value param)
{
}

CAMLexport void caml_debugger_cleanup_fork(void)
{
}

#else

#ifdef HAS_UNISTD
#include <unistd.h>
#endif
#include <errno.h>
#include <stdlib.h>
#include <sys/types.h>
#ifndef _WIN32
#include <sys/wait.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>
#else
#define ATOM ATOM_WS
#include <winsock.h>
#undef ATOM
#include <process.h>
#endif

#include "caml/fail.h"
#include "caml/fix_code.h"
#include "caml/instruct.h"
#include "caml/intext.h"
#include "caml/io.h"
#include "caml/mlvalues.h"
#include "caml/stacks.h"
#include "caml/sys.h"

static value marshal_flags = Val_emptylist;

static int sock_domain;         /* Socket domain for the debugger */
static union {                  /* Socket address for the debugger */
  struct sockaddr s_gen;
#ifndef _WIN32
  struct sockaddr_un s_unix;
#endif
  struct sockaddr_in s_inet;
} sock_addr;
static int sock_addr_len;       /* Length of sock_addr */

static int dbg_socket = -1;     /* The socket connected to the debugger */
static struct channel * dbg_in; /* Input channel on the socket */
static struct channel * dbg_out;/* Output channel on the socket */

static char *dbg_addr = NULL;

static struct skiplist event_points_table = SKIPLIST_STATIC_INITIALIZER;

static void open_connection(void)
{
#ifdef _WIN32
  /* Set socket to synchronous mode so that file descriptor-oriented
     functions (read()/write() etc.) can be used */

  int oldvalue, oldvaluelen, newvalue, retcode;
  oldvaluelen = sizeof(oldvalue);
  retcode = getsockopt(INVALID_SOCKET, SOL_SOCKET, SO_OPENTYPE,
                       (char *) &oldvalue, &oldvaluelen);
  if (retcode == 0) {
      newvalue = SO_SYNCHRONOUS_NONALERT;
      setsockopt(INVALID_SOCKET, SOL_SOCKET, SO_OPENTYPE,
                 (char *) &newvalue, sizeof(newvalue));
  }
#endif
  dbg_socket = socket(sock_domain, SOCK_STREAM, 0);
#ifdef _WIN32
  if (retcode == 0) {
    /* Restore initial mode */
    setsockopt(INVALID_SOCKET, SOL_SOCKET, SO_OPENTYPE,
               (char *) &oldvalue, oldvaluelen);
  }
#endif
  if (dbg_socket == -1 ||
      connect(dbg_socket, &sock_addr.s_gen, sock_addr_len) == -1){
    caml_fatal_error
    (
      "cannot connect to debugger at %s\n"
      "error: %s",
      (dbg_addr ? dbg_addr : "(none)"),
      strerror (errno)
    );
  }
#ifdef _WIN32
  dbg_socket = _open_osfhandle(dbg_socket, 0);
  if (dbg_socket == -1)
    caml_fatal_error("_open_osfhandle failed");
#endif
  dbg_in = caml_open_descriptor_in(dbg_socket);
  dbg_out = caml_open_descriptor_out(dbg_socket);
  /* The code in this file does not bracket channel I/O operations with
     Lock and Unlock, so fail if those are not no-ops. */
  if (caml_channel_mutex_lock != NULL ||
      caml_channel_mutex_unlock != NULL ||
      caml_channel_mutex_unlock_exn != NULL)
    caml_fatal_error("debugger does not support channel locks");
  if (!caml_debugger_in_use) caml_putword(dbg_out, -1); /* first connection */
#ifdef _WIN32
  caml_putword(dbg_out, _getpid());
#else
  caml_putword(dbg_out, getpid());
#endif
  caml_flush(dbg_out);
}

static void close_connection(void)
{
  caml_close_channel(dbg_in);
  caml_close_channel(dbg_out);
  dbg_socket = -1;              /* was closed by caml_close_channel */
}

#ifdef _WIN32
static void winsock_startup(void)
{
  WSADATA wsaData;
  int err = WSAStartup(MAKEWORD(2, 0), &wsaData);
  if (err) caml_fatal_error("WSAStartup failed");
}

static void winsock_cleanup(void)
{
  WSACleanup();
}
#endif

void caml_debugger_init(void)
{
  char * address;
  char_os * a;
  size_t a_len;
  char * port, * p;
  struct hostent * host;
  int n;

  caml_register_global_root(&marshal_flags);
  marshal_flags = caml_alloc(2, Tag_cons);
  Store_field(marshal_flags, 0, Val_int(1)); /* Marshal.Closures */
  Store_field(marshal_flags, 1, Val_emptylist);

  a = caml_secure_getenv(T("CAML_DEBUG_SOCKET"));
  address = a ? caml_stat_strdup_of_os(a) : NULL;
  if (address == NULL) return;
  if (dbg_addr != NULL) caml_stat_free(dbg_addr);
  dbg_addr = address;

  /* #8676: erase the CAML_DEBUG_SOCKET variable so that processes
     created by the program being debugged do not try to connect with
     the debugger. */
#if defined(_WIN32)
  _wputenv(L"CAML_DEBUG_SOCKET=");
#elif defined(HAS_SETENV_UNSETENV)
  unsetenv("CAML_DEBUG_SOCKET");
#endif

#ifdef _WIN32
  winsock_startup();
  (void)atexit(winsock_cleanup);
#endif
  /* Parse the address */
  port = NULL;
  for (p = address; *p != 0; p++) {
    if (*p == ':') { *p = 0; port = p+1; break; }
  }
  if (port == NULL) {
#ifndef _WIN32
    /* Unix domain */
    sock_domain = PF_UNIX;
    sock_addr.s_unix.sun_family = AF_UNIX;
    a_len = strlen(address);
    if (a_len >= sizeof(sock_addr.s_unix.sun_path)) {
      caml_fatal_error
      (
        "debug socket path length exceeds maximum permitted length"
      );
    }
    strncpy(sock_addr.s_unix.sun_path, address,
            sizeof(sock_addr.s_unix.sun_path) - 1);
    sock_addr.s_unix.sun_path[sizeof(sock_addr.s_unix.sun_path) - 1] = '\0';
    sock_addr_len =
      ((char *)&(sock_addr.s_unix.sun_path) - (char *)&(sock_addr.s_unix))
        + a_len;
#else
    caml_fatal_error("unix sockets not supported");
#endif
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
        caml_fatal_error("unknown debugging host %s", address);
      memmove(&sock_addr.s_inet.sin_addr,
              host->h_addr_list[0], host->h_length);
    }
    sock_addr.s_inet.sin_port = htons(atoi(port));
    sock_addr_len = sizeof(sock_addr.s_inet);
  }
  open_connection();
  caml_debugger_in_use = 1;
  Caml_state->trap_barrier = Caml_state->stack_high;
}

static value getval(struct channel *chan)
{
  value res;
  if (caml_really_getblock(chan, (char *) &res, sizeof(res)) < sizeof(res))
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
  saved_external_raise = Caml_state->external_raise;
  if (sigsetjmp(raise_buf.buf, 0) == 0) {
    Caml_state->external_raise = &raise_buf;
    caml_output_val(chan, val, marshal_flags);
  } else {
    /* Send wrong magic number, will cause [caml_input_value] to fail */
    caml_really_putblock(chan, "\000\000\000\000", 4);
  }
  Caml_state->external_raise = saved_external_raise;
}

static void save_instruction(code_t pc)
{
  uintnat saved;
  if (caml_skiplist_find(&event_points_table, (uintnat) pc, &saved)) {
    /* Already saved. Nothing to do. */
    return;
  }
  caml_skiplist_insert(&event_points_table, (uintnat) pc, *pc);
}

static void set_instruction(code_t pc, opcode_t opcode)
{
  save_instruction(pc);
  caml_set_instruction(pc, opcode);
}

static void restore_instruction(code_t pc)
{
  CAMLunused_start int found; CAMLunused_end
  uintnat saved;
  found = caml_skiplist_find(&event_points_table, (uintnat) pc, &saved);
  CAMLassert(found);
  *pc = saved;
  caml_skiplist_remove(&event_points_table, (uintnat) pc);
}

static code_t pc_from_pos(int frag, intnat pos)
{
  struct code_fragment *cf = caml_find_code_fragment_by_num(frag);
  CAMLassert(cf != NULL);
  return (code_t) (cf->code_start + pos);
}

opcode_t caml_debugger_saved_instruction(code_t pc)
{
  CAMLunused_start int found; CAMLunused_end
  uintnat saved;
  found = caml_skiplist_find(&event_points_table, (uintnat) pc, &saved);
  CAMLassert(found);
  return saved;
}

void caml_debugger_code_unloaded(int index)
{
  struct code_fragment *cf;
  char * pc;

  if (!caml_debugger_in_use) return;

  caml_putch(dbg_out, REP_CODE_UNLOADED);
  caml_putword(dbg_out, index);

  cf = caml_find_code_fragment_by_num(index);
  CAMLassert(cf != NULL);

  FOREACH_SKIPLIST_ELEMENT(elt, &event_points_table, {
    pc = (char *) elt->key;
    if (pc >= cf->code_start && pc < cf->code_end) {
      caml_skiplist_remove(&event_points_table, (uintnat) pc);
    }
  })
}

#define Pc(sp) ((code_t)((sp)[0]))
#define Env(sp) ((sp)[1])
#define Extra_args(sp) (Long_val(((sp)[2])))
#define Locals(sp) ((sp) + 3)

void caml_debugger(enum event_kind event, value param)
{
  value *frame, *newframe;
  intnat i, pos;
  value val;
  int frag;
  struct code_fragment *cf;

  if (dbg_socket == -1) return;  /* Not connected to a debugger. */

  /* Reset current frame */
  frame = Caml_state->extern_sp + 1;

  /* Report the event to the debugger */
  switch(event) {
  case PROGRAM_START:           /* Nothing to report */
    CAMLassert (param == Val_unit);
    goto command_loop;
  case EVENT_COUNT:
    CAMLassert (param == Val_unit);
    caml_putch(dbg_out, REP_EVENT);
    break;
  case BREAKPOINT:
    CAMLassert (param == Val_unit);
    caml_putch(dbg_out, REP_BREAKPOINT);
    break;
  case PROGRAM_EXIT:
    CAMLassert (param == Val_unit);
    caml_putch(dbg_out, REP_EXITED);
    break;
  case TRAP_BARRIER:
    CAMLassert (param == Val_unit);
    caml_putch(dbg_out, REP_TRAP);
    break;
  case UNCAUGHT_EXC:
    CAMLassert (param == Val_unit);
    caml_putch(dbg_out, REP_UNCAUGHT_EXC);
    break;
  case DEBUG_INFO_ADDED:
    caml_putch(dbg_out, REP_CODE_DEBUG_INFO);
    caml_output_val(dbg_out, /* debug_info */ param, Val_emptylist);
    break;
  case CODE_LOADED:
    caml_putch(dbg_out, REP_CODE_LOADED);
    caml_putword(dbg_out, /* index */ Long_val(param));
    break;
  case CODE_UNLOADED:
    caml_putch(dbg_out, REP_CODE_UNLOADED);
    caml_putword(dbg_out, /* index */ Long_val(param));
    break;
  }
  caml_putword(dbg_out, caml_event_count);
  if (event == EVENT_COUNT || event == BREAKPOINT) {
    caml_putword(dbg_out, Caml_state->stack_high - frame);
    cf = caml_find_code_fragment_by_pc((char*) Pc(frame));
    CAMLassert(cf != NULL);
    caml_putword(dbg_out, cf->fragnum);
    caml_putword(dbg_out, (char*) Pc(frame) - cf->code_start);
  } else {
    /* No PC and no stack frame associated with other events */
    caml_putword(dbg_out, 0);
    caml_putword(dbg_out, -1);
    caml_putword(dbg_out, 0);
  }
  caml_flush(dbg_out);

 command_loop:

  /* Read and execute the commands sent by the debugger */
  while(1) {
    switch(caml_getch(dbg_in)) {
    case REQ_SET_EVENT:
      frag = caml_getword(dbg_in);
      pos = caml_getword(dbg_in);
      set_instruction(pc_from_pos(frag, pos), EVENT);
      break;
    case REQ_SET_BREAKPOINT:
      frag = caml_getword(dbg_in);
      pos = caml_getword(dbg_in);
      set_instruction(pc_from_pos(frag, pos), BREAK);
      break;
    case REQ_RESET_INSTR:
      frag = caml_getword(dbg_in);
      pos = caml_getword(dbg_in);
      restore_instruction(pc_from_pos(frag, pos));
      break;
    case REQ_CHECKPOINT:
#ifndef _WIN32
      i = fork();
      if (i == 0) {
        close_connection();     /* Close parent connection. */
        open_connection();      /* Open new connection with debugger */
      } else {
        caml_putword(dbg_out, i);
        caml_flush(dbg_out);
      }
#else
      caml_fatal_error("REQ_CHECKPOINT command");
#endif
      break;
    case REQ_GO:
      caml_event_count = caml_getword(dbg_in);
      return;
    case REQ_STOP:
      exit(0);
      break;
    case REQ_WAIT:
#ifndef _WIN32
      wait(NULL);
#else
      caml_fatal_error("REQ_WAIT command");
#endif
      break;
    case REQ_INITIAL_FRAME:
      frame = Caml_state->extern_sp + 1;
      /* Fall through */
    case REQ_GET_FRAME:
      caml_putword(dbg_out, Caml_state->stack_high - frame);
      if (frame < Caml_state->stack_high &&
          (cf = caml_find_code_fragment_by_pc((char*) Pc(frame))) != NULL) {
        caml_putword(dbg_out, cf->fragnum);
        caml_putword(dbg_out, (char*) Pc(frame) - cf->code_start);
      } else {
        caml_putword(dbg_out, 0);
        caml_putword(dbg_out, 0);
      }
      caml_flush(dbg_out);
      break;
    case REQ_SET_FRAME:
      i = caml_getword(dbg_in);
      frame = Caml_state->stack_high - i;
      break;
    case REQ_UP_FRAME:
      i = caml_getword(dbg_in);
      newframe = frame + Extra_args(frame) + i + 3;
      if (newframe >= Caml_state->stack_high ||
          (cf = caml_find_code_fragment_by_pc((char *) Pc(newframe))) == NULL) {
        caml_putword(dbg_out, -1);
      } else {
        frame = newframe;
        caml_putword(dbg_out, Caml_state->stack_high - frame);
        caml_putword(dbg_out, cf->fragnum);
        caml_putword(dbg_out, (char*) Pc(frame) - cf->code_start);
      }
      caml_flush(dbg_out);
      break;
    case REQ_SET_TRAP_BARRIER:
      i = caml_getword(dbg_in);
      Caml_state->trap_barrier = Caml_state->stack_high - i;
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
      putval(dbg_out, *Caml_state->extern_sp);
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
        caml_putch(dbg_out, 0);
        putval(dbg_out, Field(val, i));
      } else {
        double d = Double_flat_field(val, i);
        caml_putch(dbg_out, 1);
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
      cf = caml_find_code_fragment_by_pc((char*) Code_val(val));
      CAMLassert(cf != NULL);
      caml_putword(dbg_out, cf->fragnum);
      caml_putword(dbg_out, (char*) Code_val(val) - cf->code_start);
      caml_flush(dbg_out);
      break;
    case REQ_SET_FORK_MODE:
      caml_debugger_fork_mode = caml_getword(dbg_in);
      break;
    }
  }
}

CAMLexport void caml_debugger_cleanup_fork(void)
{
  /* We could remove all of the event points, but closing the connection
   * means that they'll just be skipped anyway. */
  close_connection();
  caml_debugger_in_use = 0;
}

#endif
