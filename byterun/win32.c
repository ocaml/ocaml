/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt            */
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

/* Win32-specific stuff */

#define WIN32_LEAN_AND_MEAN
#include <wtypes.h>
#include <winbase.h>
#include <winsock2.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <io.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <ctype.h>
#include <errno.h>
#include <string.h>
#include <signal.h>
#include "caml/alloc.h"
#include "caml/address_class.h"
#include "caml/fail.h"
#include "caml/io.h"
#include "caml/memory.h"
#include "caml/misc.h"
#include "caml/osdeps.h"
#include "caml/signals.h"
#include "caml/sys.h"

#include "caml/config.h"
#ifdef SUPPORT_DYNAMIC_LINKING
#include <flexdll.h>
#endif

#ifndef S_ISREG
#define S_ISREG(mode) (((mode) & S_IFMT) == S_IFREG)
#endif

/* Very old Microsoft headers don't include intptr_t */
#if defined(_MSC_VER) && !defined(_UINTPTR_T_DEFINED)
typedef unsigned int uintptr_t;
#define _UINTPTR_T_DEFINED
#endif

CAMLnoreturn_start
static void caml_win32_sys_error (int errnum)
CAMLnoreturn_end;

static void caml_win32_sys_error(int errnum)
{
  char buffer[512];
  value msg;
  if (FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_IGNORE_INSERTS,
                    NULL,
                    errnum,
                    0,
                    buffer,
                    sizeof(buffer),
                    NULL)) {
    msg = caml_copy_string(buffer);
  } else {
    msg = caml_alloc_sprintf("unknown error #%d", errnum);
  }
  caml_raise_sys_error(msg);
}

int caml_read_fd(int fd, int flags, void * buf, int n)
{
  int retcode;
  if ((flags & CHANNEL_FLAG_FROM_SOCKET) == 0) {
    caml_enter_blocking_section();
    retcode = read(fd, buf, n);
    /* Large reads from console can fail with ENOMEM.  Reduce requested size
       and try again. */
    if (retcode == -1 && errno == ENOMEM && n > 16384) {
      retcode = read(fd, buf, 16384);
    }
    caml_leave_blocking_section();
    if (retcode == -1) caml_sys_io_error(NO_ARG);
  } else {
    caml_enter_blocking_section();
    retcode = recv((SOCKET) _get_osfhandle(fd), buf, n, 0);
    caml_leave_blocking_section();
    if (retcode == -1) caml_win32_sys_error(WSAGetLastError());
  }
  return retcode;
}

int caml_write_fd(int fd, int flags, void * buf, int n)
{
  int retcode;
  if ((flags & CHANNEL_FLAG_FROM_SOCKET) == 0) {
#if defined(NATIVE_CODE) && defined(WITH_SPACETIME)
  if (flags & CHANNEL_FLAG_BLOCKING_WRITE) {
    retcode = write(fd, buf, n);
  } else {
#endif
    caml_enter_blocking_section();
    retcode = write(fd, buf, n);
    caml_leave_blocking_section();
#if defined(NATIVE_CODE) && defined(WITH_SPACETIME)
  }
#endif
    if (retcode == -1) caml_sys_io_error(NO_ARG);
  } else {
    caml_enter_blocking_section();
    retcode = send((SOCKET) _get_osfhandle(fd), buf, n, 0);
    caml_leave_blocking_section();
    if (retcode == -1) caml_win32_sys_error(WSAGetLastError());
  }
  CAMLassert (retcode > 0);
  return retcode;
}

caml_stat_string caml_decompose_path(struct ext_table * tbl, char * path)
{
  char * p, * q;
  int n;

  if (path == NULL) return NULL;
  p = caml_stat_strdup(path);
  q = p;
  while (1) {
    for (n = 0; q[n] != 0 && q[n] != ';'; n++) /*nothing*/;
    caml_ext_table_add(tbl, q);
    q = q + n;
    if (*q == 0) break;
    *q = 0;
    q += 1;
  }
  return p;
}

caml_stat_string caml_search_in_path(struct ext_table * path, char * name)
{
  char * p, * dir, * fullname;
  int i;
  struct stat st;

  for (p = name; *p != 0; p++) {
    if (*p == '/' || *p == '\\') goto not_found;
  }
  for (i = 0; i < path->size; i++) {
    dir = path->contents[i];
    if (dir[0] == 0) continue;
         /* not sure what empty path components mean under Windows */
    fullname = caml_stat_strconcat(3, dir, "\\", name);
    caml_gc_message(0x100, "Searching %s\n", (uintnat) fullname);
    if (stat(fullname, &st) == 0 && S_ISREG(st.st_mode))
      return fullname;
    caml_stat_free(fullname);
  }
 not_found:
  caml_gc_message(0x100, "%s not found in search path\n", (uintnat) name);
  return caml_stat_strdup(name);
}

CAMLexport caml_stat_string caml_search_exe_in_path(char * name)
{
  char * fullname, * filepart;
  size_t fullnamelen;
  DWORD retcode;

  fullnamelen = strlen(name) + 1;
  if (fullnamelen < 256) fullnamelen = 256;
  while (1) {
    fullname = caml_stat_alloc(fullnamelen);
    retcode = SearchPath(NULL,              /* use system search path */
                         name,
                         ".exe",            /* add .exe extension if needed */
                         fullnamelen,
                         fullname,
                         &filepart);
    if (retcode == 0) {
      caml_gc_message(0x100, "%s not found in search path\n",
                      (uintnat) name);
      caml_stat_free(fullname);
      return caml_stat_strdup(name);
    }
    if (retcode < fullnamelen)
      return fullname;
    caml_stat_free(fullname);
    fullnamelen = retcode + 1;
  }
}

caml_stat_string caml_search_dll_in_path(struct ext_table * path, char * name)
{
  caml_stat_string dllname;
  caml_stat_string res;

  dllname = caml_stat_strconcat(2, name, ".dll");
  res = caml_search_in_path(path, dllname);
  caml_stat_free(dllname);
  return res;
}

#ifdef SUPPORT_DYNAMIC_LINKING

void * caml_dlopen(char * libname, int for_execution, int global)
{
  void *handle;
  int flags = (global ? FLEXDLL_RTLD_GLOBAL : 0);
  if (!for_execution) flags |= FLEXDLL_RTLD_NOEXEC;
  handle = flexdll_dlopen(libname, flags);
  if ((handle != NULL) && ((caml_verb_gc & 0x100) != 0)) {
    flexdll_dump_exports(handle);
    fflush(stdout);
  }
  return handle;
}

void caml_dlclose(void * handle)
{
  flexdll_dlclose(handle);
}

void * caml_dlsym(void * handle, char * name)
{
  return flexdll_dlsym(handle, name);
}

void * caml_globalsym(char * name)
{
  return flexdll_dlsym(flexdll_dlopen(NULL,0), name);
}

char * caml_dlerror(void)
{
  return flexdll_dlerror();
}

#else

void * caml_dlopen(char * libname, int for_execution, int global)
{
  return NULL;
}

void caml_dlclose(void * handle)
{
}

void * caml_dlsym(void * handle, char * name)
{
  return NULL;
}

void * caml_globalsym(char * name)
{
  return NULL;
}

char * caml_dlerror(void)
{
  return "dynamic loading not supported on this platform";
}

#endif

/* Proper emulation of signal(), including ctrl-C and ctrl-break */

typedef void (*sighandler)(int sig);
static int ctrl_handler_installed = 0;
static volatile sighandler ctrl_handler_action = SIG_DFL;

static BOOL WINAPI ctrl_handler(DWORD event)
{
  int saved_mode;

  /* Only ctrl-C and ctrl-Break are handled */
  if (event != CTRL_C_EVENT && event != CTRL_BREAK_EVENT) return FALSE;
  /* Default behavior is to exit, which we get by not handling the event */
  if (ctrl_handler_action == SIG_DFL) return FALSE;
  /* Ignore behavior is to do nothing, which we get by claiming that we
     have handled the event */
  if (ctrl_handler_action == SIG_IGN) return TRUE;
  /* Win32 doesn't like it when we do a longjmp() at this point
     (it looks like we're running in a different thread than
     the main program!).  So, just record the signal. */
  caml_record_signal(SIGINT);
  /* We have handled the event */
  return TRUE;
}

sighandler caml_win32_signal(int sig, sighandler action)
{
  sighandler oldaction;

  if (sig != SIGINT) return signal(sig, action);
  if (! ctrl_handler_installed) {
    SetConsoleCtrlHandler(ctrl_handler, TRUE);
    ctrl_handler_installed = 1;
  }
  oldaction = ctrl_handler_action;
  ctrl_handler_action = action;
  return oldaction;
}

/* Expansion of @responsefile and *? file patterns in the command line */

static int argc;
static char ** argv;
static int argvsize;

static void store_argument(char * arg);
static void expand_argument(char * arg);
static void expand_pattern(char * arg);

static void out_of_memory(void)
{
  fprintf(stderr, "Out of memory while expanding command line\n");
  exit(2);
}

static void store_argument(char * arg)
{
  if (argc + 1 >= argvsize) {
    argvsize *= 2;
    argv = (char **) caml_stat_resize_noexc(argv, argvsize * sizeof(char *));
    if (argv == NULL) out_of_memory();
  }
  argv[argc++] = arg;
}

static void expand_argument(char * arg)
{
  char * p;

  for (p = arg; *p != 0; p++) {
    if (*p == '*' || *p == '?') {
      expand_pattern(arg);
      return;
    }
  }
  store_argument(arg);
}

static void expand_pattern(char * pat)
{
  char * prefix, * p, * name;
  int handle;
  struct _finddata_t ffblk;
  size_t i;

  handle = _findfirst(pat, &ffblk);
  if (handle == -1) {
    store_argument(pat); /* a la Bourne shell */
    return;
  }
  prefix = caml_stat_strdup(pat);
  /* We need to stop at the first directory or drive boundary, because the
   * _findata_t structure contains the filename, not the leading directory. */
  for (i = strlen(prefix); i > 0; i--) {
    char c = prefix[i - 1];
    if (c == '\\' || c == '/' || c == ':') { prefix[i] = 0; break; }
  }
  /* No separator was found, it's a filename pattern without a leading directory. */
  if (i == 0)
    prefix[0] = 0;
  do {
    name = caml_stat_strconcat(2, prefix, ffblk.name);
    store_argument(name);
  } while (_findnext(handle, &ffblk) != -1);
  _findclose(handle);
  caml_stat_free(prefix);
}


CAMLexport void caml_expand_command_line(int * argcp, char *** argvp)
{
  int i;
  argc = 0;
  argvsize = 16;
  argv = (char **) caml_stat_alloc_noexc(argvsize * sizeof(char *));
  if (argv == NULL) out_of_memory();
  for (i = 0; i < *argcp; i++) expand_argument((*argvp)[i]);
  argv[argc] = NULL;
  *argcp = argc;
  *argvp = argv;
}

/* Add to [contents] the (short) names of the files contained in
   the directory named [dirname].  No entries are added for [.] and [..].
   Return 0 on success, -1 on error; set errno in the case of error. */

int caml_read_directory(char * dirname, struct ext_table * contents)
{
  size_t dirnamelen;
  char * template;
#if _MSC_VER <= 1200
  int h;
#else
  intptr_t h;
#endif
  struct _finddata_t fileinfo;

  dirnamelen = strlen(dirname);
  if (dirnamelen > 0 &&
      (dirname[dirnamelen - 1] == '/'
       || dirname[dirnamelen - 1] == '\\'
       || dirname[dirnamelen - 1] == ':'))
    template = caml_stat_strconcat(2, dirname, "*.*");
  else
    template = caml_stat_strconcat(2, dirname, "\\*.*");
  h = _findfirst(template, &fileinfo);
  if (h == -1) {
    caml_stat_free(template);
    return errno == ENOENT ? 0 : -1;
  }
  do {
    if (strcmp(fileinfo.name, ".") != 0 && strcmp(fileinfo.name, "..") != 0) {
      caml_ext_table_add(contents, caml_stat_strdup(fileinfo.name));
    }
  } while (_findnext(h, &fileinfo) == 0);
  _findclose(h);
  caml_stat_free(template);
  return 0;
}

#ifndef NATIVE_CODE

/* Set up a new thread for control-C emulation and termination */

void caml_signal_thread(void * lpParam)
{
  char *endptr;
  HANDLE h;
  /* Get an hexa-code raw handle through the environment */
  h = (HANDLE) (uintptr_t)
    strtol(caml_secure_getenv("CAMLSIGPIPE"), &endptr, 16);
  while (1) {
    DWORD numread;
    BOOL ret;
    char iobuf[2];
    /* This shall always return a single character */
    ret = ReadFile(h, iobuf, 1, &numread, NULL);
    if (!ret || numread != 1) caml_sys_exit(Val_int(2));
    switch (iobuf[0]) {
    case 'C':
      caml_record_signal(SIGINT);
      break;
    case 'T':
      raise(SIGTERM);
      return;
    }
  }
}

#endif /* NATIVE_CODE */

#if defined(NATIVE_CODE)

/* Handling of system stack overflow.
 * Based on code provided by Olivier Andrieu.

 * An EXCEPTION_STACK_OVERFLOW is signaled when the guard page at the
 * end of the stack has been accessed. Windows clears the PAGE_GUARD
 * protection (making it a regular PAGE_READWRITE) and then calls our
 * exception handler. This means that although we're handling an "out
 * of stack" condition, there is a bit of stack available to call
 * functions and allocate temporaries.
 *
 * PAGE_GUARD is a one-shot access protection mechanism: we need to
 * restore the PAGE_GUARD protection on this page otherwise the next
 * stack overflow won't be detected and the program will abruptly exit
 * with STATUS_ACCESS_VIOLATION.
 *
 * Visual Studio 2003 and later (_MSC_VER >= 1300) have a
 * _resetstkoflw() function that resets this protection.
 * Unfortunately, it cannot work when called directly from the
 * exception handler because at this point we are using the page that
 * is to be protected.
 *
 * A solution is to use an alternate stack when restoring the
 * protection. However it's not possible to use _resetstkoflw() then
 * since it determines the stack pointer by calling alloca(): it would
 * try to protect the alternate stack.
 *
 * Finally, we call caml_raise_stack_overflow; it will either call
 * caml_raise_exception which switches back to the normal stack, or
 * call caml_fatal_uncaught_exception which terminates the program
 * quickly.
 */

static uintnat win32_alt_stack[0x80];

static void caml_reset_stack (void *faulting_address)
{
  SYSTEM_INFO si;
  DWORD page_size;
  MEMORY_BASIC_INFORMATION mbi;
  DWORD oldprot;

  /* get the system's page size. */
  GetSystemInfo (&si);
  page_size = si.dwPageSize;

  /* get some information on the page the fault occurred */
  if (! VirtualQuery (faulting_address, &mbi, sizeof mbi))
    goto failed;

  VirtualProtect (mbi.BaseAddress, page_size,
                  mbi.Protect | PAGE_GUARD, &oldprot);

 failed:
  caml_raise_stack_overflow();
}


#ifndef _WIN64
static LONG CALLBACK
    caml_stack_overflow_VEH (EXCEPTION_POINTERS* exn_info)
{
  DWORD code   = exn_info->ExceptionRecord->ExceptionCode;
  CONTEXT *ctx = exn_info->ContextRecord;
  DWORD *ctx_ip = &(ctx->Eip);
  DWORD *ctx_sp = &(ctx->Esp);

  if (code == EXCEPTION_STACK_OVERFLOW && Is_in_code_area (*ctx_ip))
    {
      uintnat faulting_address;
      uintnat * alt_esp;

      /* grab the address that caused the fault */
      faulting_address = exn_info->ExceptionRecord->ExceptionInformation[1];

      /* call caml_reset_stack(faulting_address) using the alternate stack */
      alt_esp  = win32_alt_stack + sizeof(win32_alt_stack) / sizeof(uintnat);
      *--alt_esp = faulting_address;
      *ctx_sp = (uintnat) (alt_esp - 1);
      *ctx_ip = (uintnat) &caml_reset_stack;

      return EXCEPTION_CONTINUE_EXECUTION;
    }

  return EXCEPTION_CONTINUE_SEARCH;
}

#else
extern char *caml_exception_pointer;
extern value *caml_young_ptr;

/* Do not use the macro from address_class.h here. */
#undef Is_in_code_area
#define Is_in_code_area(pc) \
 ( ((char *)(pc) >= caml_code_area_start && \
    (char *)(pc) <= caml_code_area_end)     \
|| ((char *)(pc) >= &caml_system__code_begin && \
    (char *)(pc) <= &caml_system__code_end)     \
|| (Classify_addr(pc) & In_code_area) )
extern char caml_system__code_begin, caml_system__code_end;


static LONG CALLBACK
    caml_stack_overflow_VEH (EXCEPTION_POINTERS* exn_info)
{
  DWORD code   = exn_info->ExceptionRecord->ExceptionCode;
  CONTEXT *ctx = exn_info->ContextRecord;

  if (code == EXCEPTION_STACK_OVERFLOW && Is_in_code_area (ctx->Rip))
    {
      uintnat faulting_address;
      uintnat * alt_rsp;

      /* grab the address that caused the fault */
      faulting_address = exn_info->ExceptionRecord->ExceptionInformation[1];

      /* refresh runtime parameters from registers */
      caml_exception_pointer =  (char *) ctx->R14;
      caml_young_ptr         = (value *) ctx->R15;

      /* call caml_reset_stack(faulting_address) using the alternate stack */
      alt_rsp  = win32_alt_stack + sizeof(win32_alt_stack) / sizeof(uintnat);
      ctx->Rcx = faulting_address;
      ctx->Rsp = (uintnat) (alt_rsp - 4 - 1);
      ctx->Rip = (uintnat) &caml_reset_stack;

      return EXCEPTION_CONTINUE_EXECUTION;
    }

  return EXCEPTION_CONTINUE_SEARCH;
}
#endif /* _WIN64 */

void caml_win32_overflow_detection(void)
{
  AddVectoredExceptionHandler(1, caml_stack_overflow_VEH);
}

#endif /* NATIVE_CODE */

/* Seeding of pseudo-random number generators */

int caml_win32_random_seed (intnat data[16])
{
  /* For better randomness, consider:
     http://msdn.microsoft.com/library/en-us/seccrypto/security/rtlgenrandom.asp
     http://blogs.msdn.com/b/michael_howard/archive/2005/01/14/353379.aspx
  */
  FILETIME t;
  LARGE_INTEGER pc;
  GetSystemTimeAsFileTime(&t);
  QueryPerformanceCounter(&pc);  /* PR#6032 */
  data[0] = t.dwLowDateTime;
  data[1] = t.dwHighDateTime;
  data[2] = GetCurrentProcessId();
  data[3] = pc.LowPart;
  data[4] = pc.HighPart;
  return 5;
}


#if defined(_MSC_VER) && __STDC_SECURE_LIB__ >= 200411L

static void invalid_parameter_handler(const wchar_t* expression,
   const wchar_t* function,
   const wchar_t* file,
   unsigned int line,
   uintptr_t pReserved)
{
  /* no crash box */
}


void caml_install_invalid_parameter_handler()
{
  _set_invalid_parameter_handler(invalid_parameter_handler);
}

#endif


/* Recover executable name  */

char * caml_executable_name(void)
{
  char * name;
  DWORD namelen, ret;

  namelen = 256;
  while (1) {
    name = caml_stat_alloc(namelen);
    ret = GetModuleFileName(NULL, name, namelen);
    if (ret == 0) { caml_stat_free(name); return NULL; }
    if (ret < namelen) break;
    caml_stat_free(name);
    if (namelen >= 1024*1024) return NULL; /* avoid runaway and overflow */
    namelen *= 2;
  }
  return name;
}

/* snprintf emulation */

#ifdef LACKS_VSCPRINTF
/* No _vscprintf until Visual Studio .NET 2002 and sadly no version number
   in the CRT headers until Visual Studio 2005 so forced to predicate this
   on the compiler version instead */
int _vscprintf(const char * format, va_list args)
{
  int n;
  int sz = 5;
  char* buf = (char*)malloc(sz);
  n = _vsnprintf(buf, sz, format, args);
  while (n < 0 || n > sz) {
    sz += 512;
    buf = (char*)realloc(buf, sz);
    n = _vsnprintf(buf, sz, format, args);
  }
  free(buf);
  return n;
}
#endif

#if defined(_WIN32) && !defined(_UCRT)
int caml_snprintf(char * buf, size_t size, const char * format, ...)
{
  int len;
  va_list args;

  if (size > 0) {
    va_start(args, format);
    len = _vsnprintf(buf, size, format, args);
    va_end(args);
    if (len >= 0 && len < size) {
      /* [len] characters were stored in [buf],
         a null-terminator was appended. */
      return len;
    }
    /* [size] characters were stored in [buf], without null termination.
       Put a null terminator, truncating the output. */
    buf[size - 1] = 0;
  }
  /* Compute the actual length of output, excluding null terminator */
  va_start(args, format);
  len = _vscprintf(format, args);
  va_end(args);
  return len;
}
#endif

char *caml_secure_getenv (char const *var)
{
  /* Win32 doesn't have a notion of setuid bit, so getenv is safe. */
  return CAML_SYS_GETENV (var);
}
