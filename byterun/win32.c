/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*           Xavier Leroy, projet Cristal, INRIA Rocquencourt          */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

/* Win32-specific stuff */

#include <windows.h>
#include <stdlib.h>
#include <stdio.h>
#include <io.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <ctype.h>
#include <errno.h>
#include <string.h>
#include <signal.h>
#include "fail.h"
#include "memory.h"
#include "misc.h"
#include "osdeps.h"
#include "signals.h"
#include "sys.h"

#include "flexdll.h"

#ifndef S_ISREG
#define S_ISREG(mode) (((mode) & S_IFMT) == S_IFREG)
#endif

char * caml_decompose_path(struct ext_table * tbl, char * path)
{
  char * p, * q;
  int n;

  if (path == NULL) return NULL;
  p = caml_stat_alloc(strlen(path) + 1);
  strcpy(p, path);
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

char * caml_search_in_path(struct ext_table * path, char * name)
{
  char * p, * fullname;
  int i;
  struct stat st;

  for (p = name; *p != 0; p++) {
    if (*p == '/' || *p == '\\') goto not_found;
  }
  for (i = 0; i < path->size; i++) {
    fullname = caml_stat_alloc(strlen((char *)(path->contents[i])) +
                               strlen(name) + 2);
    strcpy(fullname, (char *)(path->contents[i]));
    strcat(fullname, "\\");
    strcat(fullname, name);
    caml_gc_message(0x100, "Searching %s\n", (uintnat) fullname);
    if (stat(fullname, &st) == 0 && S_ISREG(st.st_mode)) return fullname;
    caml_stat_free(fullname);
  }
 not_found:
  caml_gc_message(0x100, "%s not found in search path\n", (uintnat) name);
  fullname = caml_stat_alloc(strlen(name) + 1);
  strcpy(fullname, name);
  return fullname;
}

CAMLexport char * caml_search_exe_in_path(char * name)
{
  char * fullname, * filepart;
  DWORD pathlen, retcode;

  pathlen = strlen(name) + 1;
  if (pathlen < 256) pathlen = 256;
  while (1) {
    fullname = stat_alloc(pathlen);
    retcode = SearchPath(NULL,              /* use system search path */
                         name,
                         ".exe",            /* add .exe extension if needed */
                         pathlen,
                         fullname,
                         &filepart);
    if (retcode == 0) {
      caml_gc_message(0x100, "%s not found in search path\n",
                      (uintnat) name);
      strcpy(fullname, name);
      break;
    }
    if (retcode < pathlen) break;
    stat_free(fullname);
    pathlen = retcode + 1;
  }
  return fullname;
}

char * caml_search_dll_in_path(struct ext_table * path, char * name)
{
  char * dllname = caml_stat_alloc(strlen(name) + 5);
  char * res;
  strcpy(dllname, name);
  strcat(dllname, ".dll");
  res = caml_search_in_path(path, dllname);
  caml_stat_free(dllname);
  return res;
}

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
    argv = (char **) realloc(argv, argvsize * sizeof(char *));
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
  int handle;
  struct _finddata_t ffblk;
  int preflen;

  handle = _findfirst(pat, &ffblk);
  if (handle == -1) {
    store_argument(pat); /* a la Bourne shell */
    return;
  }
  for (preflen = strlen(pat); preflen > 0; preflen--) {
    char c = pat[preflen - 1];
    if (c == '\\' || c == '/' || c == ':') break;
  }
  do {
    char * name = malloc(preflen + strlen(ffblk.name) + 1);
    if (name == NULL) out_of_memory();
    memcpy(name, pat, preflen);
    strcpy(name + preflen, ffblk.name);
    store_argument(name);
  } while (_findnext(handle, &ffblk) != -1);
  _findclose(handle);
}


CAMLexport void caml_expand_command_line(int * argcp, char *** argvp)
{
  int i;
  argc = 0;
  argvsize = 16;
  argv = (char **) malloc(argvsize * sizeof(char *));
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
  int dirnamelen;
  char * template;
#if _MSC_VER <= 1200
  int h;
#else
  intptr_t h;
#endif
  struct _finddata_t fileinfo;
  char * p;

  dirnamelen = strlen(dirname);
  template = caml_stat_alloc(dirnamelen + 5);
  strcpy(template, dirname);
  switch (dirname[dirnamelen - 1]) {
  case '/': case '\\': case ':':
    strcat(template, "*.*"); break;
  default:
    strcat(template, "\\*.*");
  }
  h = _findfirst(template, &fileinfo);
  caml_stat_free(template);
  if (h == -1) return errno == ENOENT ? 0 : -1;
  do {
    if (strcmp(fileinfo.name, ".") != 0 && strcmp(fileinfo.name, "..") != 0) {
      p = caml_stat_alloc(strlen(fileinfo.name) + 1);
      strcpy(p, fileinfo.name);
      caml_ext_table_add(contents, p);
    }
  } while (_findnext(h, &fileinfo) == 0);
  _findclose(h);
  return 0;
}

#ifndef NATIVE_CODE

/* Set up a new thread for control-C emulation and termination */

void caml_signal_thread(void * lpParam)
{
  char *endptr;
  HANDLE h;
  /* Get an hexa-code raw handle through the environment */
  h = (HANDLE) strtol(getenv("CAMLSIGPIPE"), &endptr, 16);
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

#if defined(NATIVE_CODE) && !defined(_WIN64)

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
 * A solution is to used an alternate stack when restoring the
 * protection. However it's not possible to use _resetstkoflw() then
 * since it determines the stack pointer by calling alloca(): it would
 * try to protect the alternate stack.
 *
 * Finally, we call caml_raise_stack_overflow; it will either call
 * caml_raise_exception which switches back to the normal stack, or
 * call caml_fatal_uncaught_exception which terminates the program
 * quickly.
 *
 * NB: The PAGE_GUARD protection is only available on WinNT, not
 * Win9x. There is an equivalent mechanism on Win9x with
 * PAGE_NOACCESS.
 *
 * Currently, does not work under Win64.
 */

static uintnat win32_alt_stack[0x80];

static void caml_reset_stack (void *faulting_address)
{
  OSVERSIONINFO osi;
  SYSTEM_INFO si;
  DWORD page_size;
  MEMORY_BASIC_INFORMATION mbi;
  DWORD oldprot;

  /* get the os version (Win9x or WinNT ?) */
  osi.dwOSVersionInfoSize = sizeof osi;
  if (! GetVersionEx (&osi))
    goto failed;

  /* get the system's page size. */
  GetSystemInfo (&si);
  page_size = si.dwPageSize;

  /* get some information on the page the fault occurred */
  if (! VirtualQuery (faulting_address, &mbi, sizeof mbi))
    goto failed;

  /* restore the PAGE_GUARD protection on this page */
  switch (osi.dwPlatformId) {
  case VER_PLATFORM_WIN32_NT:
    VirtualProtect (mbi.BaseAddress, page_size,
                    mbi.Protect | PAGE_GUARD, &oldprot);
    break;
  case VER_PLATFORM_WIN32_WINDOWS:
    VirtualProtect (mbi.BaseAddress, page_size,
                    PAGE_NOACCESS, &oldprot);
    break;
  }

 failed:
  caml_raise_stack_overflow();
}

extern char * caml_code_area_start, * caml_code_area_end;
CAMLextern int caml_is_in_code(void *);

#define Is_in_code_area(pc) \
 ( ((char *)(pc) >= caml_code_area_start && \
    (char *)(pc) <= caml_code_area_end)     \
   || (Classify_addr(pc) & In_code_area) )

static LONG CALLBACK
    caml_UnhandledExceptionFilter (EXCEPTION_POINTERS* exn_info)
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

void caml_win32_overflow_detection()
{
  SetUnhandledExceptionFilter (caml_UnhandledExceptionFilter);
}

#endif

/* Seeding of pseudo-random number generators */

int caml_win32_random_seed (intnat data[16])
{
  /* For better randomness, consider:
     http://msdn.microsoft.com/library/en-us/seccrypto/security/rtlgenrandom.asp
  */
  FILETIME t;
  GetSystemTimeAsFileTime(&t);
  data[0] = t.dwLowDateTime;
  data[1] = t.dwHighDateTime;
  data[2] = GetCurrentProcessId();
  return 3;
}
