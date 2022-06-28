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

/* FILE_INFO_BY_HANDLE_CLASS and FILE_NAME_INFO are only available from Windows
   Vista onwards */
#undef _WIN32_WINNT
#define _WIN32_WINNT 0x0600

#define WIN32_LEAN_AND_MEAN
#include <wtypes.h>
#include <winbase.h>
#include <winsock2.h>
#include <winioctl.h>
#include <direct.h>
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
#if defined(DEBUG) || defined(NATIVE_CODE)
#include <dbghelp.h>
#endif
#include "caml/alloc.h"
#include "caml/codefrag.h"
#include "caml/fail.h"
#include "caml/io.h"
#include "caml/memory.h"
#include "caml/misc.h"
#include "caml/osdeps.h"
#include "caml/signals.h"
#include "caml/sys.h"
#include "caml/winsupport.h"
#include "caml/startup_aux.h"
#include "caml/platform.h"

#include "caml/config.h"

#if defined(SUPPORT_DYNAMIC_LINKING) && !defined(BUILDING_LIBCAMLRUNS)
#define WITH_DYNAMIC_LINKING
#endif

#ifdef WITH_DYNAMIC_LINKING
#include <flexdll.h>
#endif

#ifndef S_ISREG
#define S_ISREG(mode) (((mode) & S_IFMT) == S_IFREG)
#endif

unsigned short caml_win32_major = 0;
unsigned short caml_win32_minor = 0;
unsigned short caml_win32_build = 0;
unsigned short caml_win32_revision = 0;

CAMLnoreturn_start
static void caml_win32_sys_error (int errnum)
CAMLnoreturn_end;

static void caml_win32_sys_error(int errnum)
{
  wchar_t buffer[512];
  value msg;
  if (FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_IGNORE_INSERTS,
                    NULL,
                    errnum,
                    0,
                    buffer,
                    sizeof(buffer)/sizeof(wchar_t),
                    NULL)) {
    msg = caml_copy_string_of_utf16(buffer);
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
    caml_enter_blocking_section();
    retcode = write(fd, buf, n);
    caml_leave_blocking_section();
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

wchar_t * caml_decompose_path(struct ext_table * tbl, wchar_t * path)
{
  wchar_t * p, * q;
  int n;

  if (path == NULL) return NULL;
  p = caml_stat_wcsdup(path);
  q = p;
  while (1) {
    for (n = 0; q[n] != 0 && q[n] != L';'; n++) /*nothing*/;
    caml_ext_table_add(tbl, q);
    q = q + n;
    if (*q == 0) break;
    *q = 0;
    q += 1;
  }
  return p;
}

wchar_t * caml_search_in_path(struct ext_table * path, const wchar_t * name)
{
  wchar_t * dir, * fullname;
  char * u8;
  const wchar_t * p;
  int i;
  struct _stati64 st;

  for (p = name; *p != 0; p++) {
    if (*p == '/' || *p == '\\') goto not_found;
  }
  for (i = 0; i < path->size; i++) {
    dir = path->contents[i];
    if (dir[0] == 0) continue;
         /* not sure what empty path components mean under Windows */
    fullname = caml_stat_wcsconcat(3, dir, L"\\", name);
    u8 = caml_stat_strdup_of_utf16(fullname);
    caml_gc_message(0x100, "Searching %s\n", u8);
    caml_stat_free(u8);
    if (_wstati64(fullname, &st) == 0 && S_ISREG(st.st_mode))
      return fullname;
    caml_stat_free(fullname);
  }
 not_found:
  u8 = caml_stat_strdup_of_utf16(name);
  caml_gc_message(0x100, "%s not found in search path\n", u8);
  caml_stat_free(u8);
  return caml_stat_wcsdup(name);
}

CAMLexport wchar_t * caml_search_exe_in_path(const wchar_t * name)
{
  wchar_t * fullname, * filepart;
  char * u8;
  size_t fullnamelen;
  DWORD retcode;

  fullnamelen = wcslen(name) + 1;
  if (fullnamelen < 256) fullnamelen = 256;
  while (1) {
    fullname = caml_stat_alloc(fullnamelen*sizeof(wchar_t));
    retcode = SearchPath(NULL,              /* use system search path */
                         name,
                         L".exe",            /* add .exe extension if needed */
                         fullnamelen,
                         fullname,
                         &filepart);
    if (retcode == 0) {
      u8 = caml_stat_strdup_of_utf16(name);
      caml_gc_message(0x100, "%s not found in search path\n", u8);
      caml_stat_free(u8);
      caml_stat_free(fullname);
      return caml_stat_strdup_os(name);
    }
    if (retcode < fullnamelen)
      return fullname;
    caml_stat_free(fullname);
    fullnamelen = retcode + 1;
  }
}

wchar_t * caml_search_dll_in_path(struct ext_table * path, const wchar_t * name)
{
  wchar_t * dllname;
  wchar_t * res;

  dllname = caml_stat_wcsconcat(2, name, L".dll");
  res = caml_search_in_path(path, dllname);
  caml_stat_free(dllname);
  return res;
}

#ifdef WITH_DYNAMIC_LINKING

void * caml_dlopen(wchar_t * libname, int for_execution, int global)
{
  void *handle;
  int flags = (global ? FLEXDLL_RTLD_GLOBAL : 0);
  if (!for_execution) flags |= FLEXDLL_RTLD_NOEXEC;
  handle = flexdll_wdlopen(libname, flags);
  if ((handle != NULL) && ((caml_params->verb_gc & 0x100) != 0)) {
    flexdll_dump_exports(handle);
    fflush(stdout);
  }
  return handle;
}

void caml_dlclose(void * handle)
{
  flexdll_dlclose(handle);
}

void * caml_dlsym(void * handle, const char * name)
{
  return flexdll_dlsym(handle, name);
}

void * caml_globalsym(const char * name)
{
  return flexdll_dlsym(flexdll_wdlopen(NULL,0), name);
}

char * caml_dlerror(void)
{
  return flexdll_dlerror();
}

#else

void * caml_dlopen(wchar_t * libname, int for_execution, int global)
{
  return NULL;
}

void caml_dlclose(void * handle)
{
}

void * caml_dlsym(void * handle, const char * name)
{
  return NULL;
}

void * caml_globalsym(const char * name)
{
  return NULL;
}

char * caml_dlerror(void)
{
  return "dynamic loading not supported on this platform";
}

#endif /* WITH_DYNAMIC_LINKING */

/* Proper emulation of signal(), including ctrl-C and ctrl-break */

typedef void (*sighandler)(int sig);
static int ctrl_handler_installed = 0;
static volatile sighandler ctrl_handler_action = SIG_DFL;

static BOOL WINAPI ctrl_handler(DWORD event)
{
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
static wchar_t ** argv;
static int argvsize;

static void store_argument(wchar_t * arg);
static void expand_argument(wchar_t * arg);
static void expand_pattern(wchar_t * arg);

static void out_of_memory(void)
{
  caml_fatal_error("out of memory while expanding command line");
}

static void store_argument(wchar_t * arg)
{
  if (argc + 1 >= argvsize) {
    argvsize *= 2;
    argv =
      (wchar_t **) caml_stat_resize_noexc(argv, argvsize * sizeof(wchar_t *));
    if (argv == NULL) out_of_memory();
  }
  argv[argc++] = arg;
}

static void expand_argument(wchar_t * arg)
{
  wchar_t * p;

  for (p = arg; *p != 0; p++) {
    if (*p == L'*' || *p == L'?') {
      expand_pattern(arg);
      return;
    }
  }
  store_argument(arg);
}

static void expand_pattern(wchar_t * pat)
{
  wchar_t * prefix, * p, * name;
  intptr_t handle;
  struct _wfinddata_t ffblk;
  size_t i;

  handle = _wfindfirst(pat, &ffblk);
  if (handle == -1) {
    store_argument(pat); /* a la Bourne shell */
    return;
  }
  prefix = caml_stat_wcsdup(pat);
  /* We need to stop at the first directory or drive boundary, because the
   * _findata_t structure contains the filename, not the leading directory. */
  for (i = wcslen(prefix); i > 0; i--) {
    wchar_t c = prefix[i - 1];
    if (c == L'\\' || c == L'/' || c == L':') { prefix[i] = 0; break; }
  }
  /* No separator was found, it's a filename pattern without a leading
     directory. */
  if (i == 0)
    prefix[0] = 0;
  do {
    name = caml_stat_wcsconcat(2, prefix, ffblk.name);
    store_argument(name);
  } while (_wfindnext(handle, &ffblk) != -1);
  _findclose(handle);
  caml_stat_free(prefix);
}


CAMLexport void caml_expand_command_line(int * argcp, wchar_t *** argvp)
{
  int i;
  argc = 0;
  argvsize = 16;
  argv = (wchar_t **) caml_stat_alloc_noexc(argvsize * sizeof(wchar_t *));
  if (argv == NULL) out_of_memory();
  for (i = 0; i < *argcp; i++) expand_argument((*argvp)[i]);
  argv[argc] = NULL;
  *argcp = argc;
  *argvp = argv;
}

/* Add to [contents] the (short) names of the files contained in
   the directory named [dirname].  No entries are added for [.] and [..].
   Return 0 on success, -1 on error; set errno in the case of error. */

CAMLexport int caml_read_directory(wchar_t * dirname,
                                   struct ext_table * contents)
{
  size_t dirnamelen;
  wchar_t * template;
  intptr_t h;
  struct _wfinddata_t fileinfo;

  dirnamelen = wcslen(dirname);
  if (dirnamelen > 0 &&
      (dirname[dirnamelen - 1] == L'/'
       || dirname[dirnamelen - 1] == L'\\'
       || dirname[dirnamelen - 1] == L':'))
    template = caml_stat_wcsconcat(2, dirname, L"*.*");
  else
    template = caml_stat_wcsconcat(2, dirname, L"\\*.*");
  h = _wfindfirst(template, &fileinfo);
  if (h == -1) {
    caml_stat_free(template);
    return errno == ENOENT ? 0 : -1;
  }
  do {
    if (wcscmp(fileinfo.name, L".") != 0 && wcscmp(fileinfo.name, L"..") != 0) {
      caml_ext_table_add(contents, caml_stat_strdup_of_utf16(fileinfo.name));
    }
  } while (_wfindnext(h, &fileinfo) == 0);
  _findclose(h);
  caml_stat_free(template);
  return 0;
}

#ifndef NATIVE_CODE

/* Set up a new thread for control-C emulation and termination */

void caml_signal_thread(void * lpParam)
{
  wchar_t *endptr;
  HANDLE h;
  /* Get an hexa-code raw handle through the environment */
  h = (HANDLE) (uintptr_t)
    wcstol(caml_secure_getenv(T("CAMLSIGPIPE")), &endptr, 16);
  while (1) {
    DWORD numread;
    BOOL ret;
    char iobuf[2];
    /* This shall always return a single character */
    ret = ReadFile(h, iobuf, 1, &numread, NULL);
    if (!ret || numread != 1) caml_do_exit(2);
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

static uintnat win32_alt_stack[0x100];

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

  if (code == EXCEPTION_STACK_OVERFLOW &&
      caml_find_code_fragment_by_pc((char *) (*ctx_ip)) != NULL)
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

static LONG CALLBACK
    caml_stack_overflow_VEH (EXCEPTION_POINTERS* exn_info)
{
  DWORD code   = exn_info->ExceptionRecord->ExceptionCode;
  CONTEXT *ctx = exn_info->ContextRecord;

  if (code == EXCEPTION_STACK_OVERFLOW &&
      caml_find_code_fragment_by_pc((char *) (ctx->Rip)) != NULL)
    {
      uintnat faulting_address;
      uintnat * alt_rsp;

      /* grab the address that caused the fault */
      faulting_address = exn_info->ExceptionRecord->ExceptionInformation[1];

      /* refresh runtime parameters from registers */
      Caml_state->young_ptr = (value *) ctx->R15;

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

static PVOID caml_stack_overflow_handle;

void caml_win32_overflow_detection(void)
{
  caml_stack_overflow_handle =
    AddVectoredExceptionHandler(1, caml_stack_overflow_VEH);
  if (caml_stack_overflow_handle == NULL) {
    caml_fatal_error("cannot install stack overflow detection");
  }
}

void caml_win32_unregister_overflow_detection(void)
{
  RemoveVectoredExceptionHandler(caml_stack_overflow_handle);
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


#ifdef _MSC_VER

static void invalid_parameter_handler(const wchar_t* expression,
   const wchar_t* function,
   const wchar_t* file,
   unsigned int line,
   uintptr_t pReserved)
{
  /* no crash box */
}


void caml_install_invalid_parameter_handler(void)
{
  _set_invalid_parameter_handler(invalid_parameter_handler);
}

#endif


/* Recover executable name  */

wchar_t * caml_executable_name(void)
{
  wchar_t * name;
  DWORD namelen, ret;

  namelen = 256;
  while (1) {
    name = caml_stat_alloc(namelen*sizeof(wchar_t));
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

#define CAML_SNPRINTF(_vsnprintf, _vscprintf) \
{ \
  int len; \
  va_list args; \
\
  if (size > 0) { \
    va_start(args, format); \
    len = _vsnprintf(buf, size, format, args); \
    va_end(args); \
    if (len >= 0 && len < size) { \
      /* [len] characters were stored in [buf], \
         a null-terminator was appended. */ \
      return len; \
    } \
    /* [size] characters were stored in [buf], without null termination. \
       Put a null terminator, truncating the output. */ \
    buf[size - 1] = 0; \
  } \
  /* Compute the actual length of output, excluding null terminator */ \
  va_start(args, format); \
  len = _vscprintf(format, args); \
  va_end(args); \
  return len; \
}

#ifndef _UCRT
int caml_snprintf(char * buf, size_t size, const char * format, ...)
CAML_SNPRINTF(_vsnprintf, _vscprintf)
#endif

int caml_snwprintf(wchar_t * buf, size_t size, const wchar_t * format, ...)
CAML_SNPRINTF(_vsnwprintf, _vscwprintf)

#undef CAML_SNPRINTF

wchar_t *caml_secure_getenv (wchar_t const *var)
{
  /* Win32 doesn't have a notion of setuid bit, so getenv is safe. */
  return _wgetenv(var);
}

/* caml_win32_getenv is used to implement Sys.getenv and Unix.getenv in such a
   way that they get direct access to the Win32 environment rather than to the
   copy that is cached by the C runtime system. The result of caml_win32_getenv
   is dynamically allocated and must be explicitly deallocated.

   In contrast, the OCaml runtime system still calls _wgetenv from the C runtime
   system, via caml_secure_getenv. The result is statically allocated and needs
   no deallocation. */
CAMLexport wchar_t *caml_win32_getenv(wchar_t const *lpName)
{
  wchar_t * lpBuffer;
  DWORD nSize = 256, res;

  lpBuffer = caml_stat_alloc_noexc(nSize * sizeof(wchar_t));

  if (lpBuffer == NULL)
    return NULL;

  res = GetEnvironmentVariable(lpName, lpBuffer, nSize);

  if (res == 0) {
    caml_stat_free(lpBuffer);
    return NULL;
  }

  if (res < nSize)
    return lpBuffer;

  nSize = res;
  lpBuffer = caml_stat_resize_noexc(lpBuffer, nSize * sizeof(wchar_t));

  if (lpBuffer == NULL)
    return NULL;

  res = GetEnvironmentVariable(lpName, lpBuffer, nSize);

  if (res == 0 || res >= nSize) {
    caml_stat_free(lpBuffer);
    return NULL;
  }

  return lpBuffer;
}

/* The rename() implementation in MSVC's CRT is based on MoveFile()
   and therefore fails if the new name exists.  This is inconsistent
   with POSIX and a problem in practice.  Here we reimplement
   rename() using MoveFileEx() to make it more POSIX-like.
   There are no official guarantee that the rename operation is atomic,
   but it is widely believed to be atomic on NTFS. */

int caml_win32_rename(const wchar_t * oldpath, const wchar_t * newpath)
{
  /* MOVEFILE_REPLACE_EXISTING: to be closer to POSIX
     MOVEFILE_COPY_ALLOWED: MoveFile performs a copy if old and new
       paths are on different devices, so we do the same here for
       compatibility with the old rename()-based implementation.
     MOVEFILE_WRITE_THROUGH: not sure it's useful; affects only
       the case where a copy is done. */
  if (MoveFileEx(oldpath, newpath,
                 MOVEFILE_REPLACE_EXISTING | MOVEFILE_WRITE_THROUGH |
                 MOVEFILE_COPY_ALLOWED)) {
    return 0;
  }
  /* Modest attempt at mapping Win32 error codes to POSIX error codes.
     The __dosmaperr() function from the CRT does a better job but is
     generally not accessible. */
  switch (GetLastError()) {
  case ERROR_FILE_NOT_FOUND: case ERROR_PATH_NOT_FOUND:
    errno = ENOENT; break;
  case ERROR_ACCESS_DENIED: case ERROR_WRITE_PROTECT: case ERROR_CANNOT_MAKE:
    errno = EACCES; break;
  case ERROR_CURRENT_DIRECTORY: case ERROR_BUSY:
    errno = EBUSY; break;
  case ERROR_NOT_SAME_DEVICE:
    errno = EXDEV; break;
  case ERROR_ALREADY_EXISTS:
    errno = EEXIST; break;
  default:
    errno = EINVAL;
  }
  return -1;
}

int caml_win32_unlink(const wchar_t * path) {
  int ret;

  ret = _wunlink(path);
  /* On Windows, trying to unlink a symlink to a directory will return
   * EACCES, but the symlink can be deleted with rmdir. */
  if (ret == -1 && errno == EACCES) {
    HANDLE h;
    DWORD attrs, dummy;
    union {
      char raw[16384];
      REPARSE_DATA_BUFFER point;
    } buffer;

    attrs = GetFileAttributes(path);
    if (attrs == INVALID_FILE_ATTRIBUTES ||
        !(attrs & (FILE_ATTRIBUTE_DIRECTORY | FILE_ATTRIBUTE_REPARSE_POINT)))
      return -1;

    h = CreateFile(path,
                   FILE_READ_ATTRIBUTES,
                   FILE_SHARE_DELETE | FILE_SHARE_READ | FILE_SHARE_WRITE,
                   NULL,
                   OPEN_EXISTING,
                   FILE_FLAG_BACKUP_SEMANTICS | FILE_FLAG_OPEN_REPARSE_POINT,
                   NULL);
    if (h == INVALID_HANDLE_VALUE)
      return -1;

    ret = DeviceIoControl(h, FSCTL_GET_REPARSE_POINT, NULL, 0, &buffer.point,
                          sizeof(buffer.raw), &dummy, NULL);
    CloseHandle(h);
    if (!ret || buffer.point.ReparseTag != IO_REPARSE_TAG_SYMLINK)
      return -1;

    ret = _wrmdir(path);
    if (ret == -1)
      errno = EACCES;
  }
  return ret;
}

/* Windows Unicode support */
static uintnat windows_unicode_enabled = WINDOWS_UNICODE;

/* If [windows_unicode_strict] is non-zero, then illegal UTF-8 characters (on
   the OCaml side) or illegal UTF-16 characters (on the Windows side) cause an
   error to be signaled.  What happens then depends on the variable
   [windows_unicode_fallback].

   If [windows_unicode_strict] is zero, then illegal characters are silently
   dropped. */
static uintnat windows_unicode_strict = 1;

/* If [windows_unicode_fallback] is non-zero, then if an error is signaled when
   translating to UTF-16, the translation is re-done under the assumption that
   the argument string is encoded in the local codepage. */
static uintnat windows_unicode_fallback = 1;

CAMLexport int caml_win32_multi_byte_to_wide_char(const char *s, int slen,
                                                  wchar_t *out, int outlen)
{
  int retcode;

  CAMLassert (s != NULL);

  if (slen == 0)
    return 0;

  if (windows_unicode_enabled != 0) {
    retcode =
      MultiByteToWideChar(CP_UTF8,
                          windows_unicode_strict ? MB_ERR_INVALID_CHARS : 0,
                          s, slen, out, outlen);
    if (retcode == 0 && windows_unicode_fallback != 0)
      retcode = MultiByteToWideChar(CP_ACP, 0, s, slen, out, outlen);
  } else {
    retcode = MultiByteToWideChar(CP_ACP, 0, s, slen, out, outlen);
  }

  if (retcode == 0)
    caml_win32_sys_error(GetLastError());

  return retcode;
}

/* For old versions of Windows we simply ignore the flag */
#ifndef WC_ERR_INVALID_CHARS
#define WC_ERR_INVALID_CHARS 0
#endif

CAMLexport int caml_win32_wide_char_to_multi_byte(const wchar_t *s, int slen,
                                                  char *out, int outlen)
{
  int retcode;

  CAMLassert(s != NULL);

  if (slen == 0)
    return 0;

  if (windows_unicode_enabled != 0)
    retcode =
      WideCharToMultiByte(CP_UTF8,
                          windows_unicode_strict ? WC_ERR_INVALID_CHARS : 0,
                          s, slen, out, outlen, NULL, NULL);
  else
    retcode =
      WideCharToMultiByte(CP_ACP, 0, s, slen, out, outlen, NULL, NULL);

  if (retcode == 0)
    caml_win32_sys_error(GetLastError());

  return retcode;
}

CAMLexport value caml_copy_string_of_utf16(const wchar_t *s)
{
  int retcode, slen;
  value v;

  slen = wcslen(s);
  /* Do not include final NULL */
  retcode = caml_win32_wide_char_to_multi_byte(s, slen, NULL, 0);
  v = caml_alloc_string(retcode);
  caml_win32_wide_char_to_multi_byte(s, slen, (char *)String_val(v), retcode);

  return v;
}

CAMLexport wchar_t* caml_stat_strdup_to_utf16(const char *s)
{
  wchar_t * ws;
  int retcode;

  retcode = caml_win32_multi_byte_to_wide_char(s, -1, NULL, 0);
  ws = caml_stat_alloc_noexc(retcode * sizeof(*ws));
  caml_win32_multi_byte_to_wide_char(s, -1, ws, retcode);

  return ws;
}

CAMLexport caml_stat_string caml_stat_strdup_of_utf16(const wchar_t *s)
{
  caml_stat_string out;
  int retcode;

  retcode = caml_win32_wide_char_to_multi_byte(s, -1, NULL, 0);
  out = caml_stat_alloc(retcode);
  caml_win32_wide_char_to_multi_byte(s, -1, out, retcode);

  return out;
}

void caml_probe_win32_version(void)
{
  /* Determine the version of Windows we're running, and cache it */
  WCHAR fileName[MAX_PATH];
  DWORD size =
    GetModuleFileName(GetModuleHandle(L"kernel32"), fileName, MAX_PATH);
  DWORD dwHandle = 0;
  BYTE* versionInfo;
  fileName[size] = 0;
  size = GetFileVersionInfoSize(fileName, &dwHandle);
  versionInfo = (BYTE*)malloc(size * sizeof(BYTE));
  if (GetFileVersionInfo(fileName, 0, size, versionInfo)) {
    UINT len = 0;
    VS_FIXEDFILEINFO* vsfi = NULL;
    VerQueryValue(versionInfo, L"\\", (void**)&vsfi, &len);
    caml_win32_major = HIWORD(vsfi->dwProductVersionMS);
    caml_win32_minor = LOWORD(vsfi->dwProductVersionMS);
    caml_win32_build = HIWORD(vsfi->dwProductVersionLS);
    caml_win32_revision = LOWORD(vsfi->dwProductVersionLS);
  }
  free(versionInfo);
}

static UINT startup_codepage = 0;

void caml_setup_win32_terminal(void)
{
  if (caml_win32_major >= 10) {
    startup_codepage = GetConsoleOutputCP();
    if (startup_codepage != CP_UTF8)
      SetConsoleOutputCP(CP_UTF8);
  }
}

void caml_restore_win32_terminal(void)
{
  if (startup_codepage != 0)
    SetConsoleOutputCP(startup_codepage);
}

/* Detect if a named pipe corresponds to a Cygwin/MSYS pty: see
   https://github.com/mirror/newlib-cygwin/blob/00e9bf2/winsup/cygwin/dtable.cc#L932
*/
typedef
BOOL (WINAPI *tGetFileInformationByHandleEx)(HANDLE, FILE_INFO_BY_HANDLE_CLASS,
                                             LPVOID, DWORD);

static int caml_win32_is_cygwin_pty(HANDLE hFile)
{
  char buffer[1024];
  FILE_NAME_INFO * nameinfo = (FILE_NAME_INFO *) buffer;
  static tGetFileInformationByHandleEx pGetFileInformationByHandleEx =
    INVALID_HANDLE_VALUE;

  if (pGetFileInformationByHandleEx == INVALID_HANDLE_VALUE)
    pGetFileInformationByHandleEx =
      (tGetFileInformationByHandleEx)GetProcAddress(
        GetModuleHandle(L"KERNEL32.DLL"), "GetFileInformationByHandleEx");

  if (pGetFileInformationByHandleEx == NULL)
    return 0;

  /* Get pipe name. GetFileInformationByHandleEx does not NULL-terminate the
     string, so reduce the buffer size to allow for adding one. */
  if (! pGetFileInformationByHandleEx(hFile,
                                      FileNameInfo,
                                      buffer,
                                      sizeof(buffer) - sizeof(WCHAR)))
    return 0;

  nameinfo->FileName[nameinfo->FileNameLength / sizeof(WCHAR)] = L'\0';

  /* check if this could be a msys pty pipe ('msys-XXXX-ptyN-XX')
     or a cygwin pty pipe ('cygwin-XXXX-ptyN-XX') */
  if ((wcsstr(nameinfo->FileName, L"msys-") ||
       wcsstr(nameinfo->FileName, L"cygwin-")) &&
         wcsstr(nameinfo->FileName, L"-pty"))
    return 1;

  return 0;
}

CAMLexport int caml_win32_isatty(int fd)
{
  DWORD lpMode;
  HANDLE hFile = (HANDLE)_get_osfhandle(fd);

  if (hFile == INVALID_HANDLE_VALUE)
    return 0;

  switch (GetFileType(hFile)) {
    case FILE_TYPE_CHAR:
      /* Both console handles and the NUL device are FILE_TYPE_CHAR.  The NUL
         device returns FALSE for a GetConsoleMode call. _isatty incorrectly
         only uses GetFileType (see GPR#1321). */
      return GetConsoleMode(hFile, &lpMode);
    case FILE_TYPE_PIPE:
      /* Cygwin PTYs are implemented using named pipes */
      return caml_win32_is_cygwin_pty(hFile);
    default:
      break;
  }

  return 0;
}

int caml_num_rows_fd(int fd)
{
  return -1;
}

/* UCRT clock function returns wall-clock time */
CAMLexport clock_t caml_win32_clock(void)
{
  FILETIME c, e, stime, utime;
  ULARGE_INTEGER tmp;
  ULONGLONG total, clocks_per_sec;

  if (!(GetProcessTimes(GetCurrentProcess(), &c, &e, &stime, &utime))) {
    return (clock_t)(-1);
  }

  tmp.u.LowPart = stime.dwLowDateTime;
  tmp.u.HighPart = stime.dwHighDateTime;
  total = tmp.QuadPart;
  tmp.u.LowPart = utime.dwLowDateTime;
  tmp.u.HighPart = utime.dwHighDateTime;
  total += tmp.QuadPart;

  /* total in 100-nanosecond intervals (1e7 / CLOCKS_PER_SEC) */
  clocks_per_sec = INT64_LITERAL(10000000U) / (ULONGLONG)CLOCKS_PER_SEC;
  return (clock_t)(total / clocks_per_sec);
}

static LARGE_INTEGER frequency;
static LARGE_INTEGER clock_offset;
typedef void (WINAPI *LPFN_GETSYSTEMTIME) (LPFILETIME);

void caml_init_os_params(void)
{
  SYSTEM_INFO si;
  LPFN_GETSYSTEMTIME pGetSystemTime;
  FILETIME stamp;
  ULARGE_INTEGER now;
  LARGE_INTEGER counter;

  /* Get the system page size */
  GetSystemInfo(&si);
  caml_sys_pagesize = si.dwPageSize;

  /* Get the number of nanoseconds for each tick in QueryPerformanceCounter */
  QueryPerformanceFrequency(&frequency);
  /* Convert the frequency to the duration of 1 tick in ns */
  frequency.QuadPart = 1000000000LL / frequency.QuadPart;

  /* Get the current time as accurately as we can.
     GetSystemTimePreciseAsFileTime is available on Windows 8 / Server 2012+ and
     gives <1us precision. For Windows 7 and earlier, which is only accurate to
     10-100ms. */
  pGetSystemTime =
    (LPFN_GETSYSTEMTIME)GetProcAddress(GetModuleHandle(L"kernel32"),
                                       "GetSystemTimePreciseAsFileTime");
  if (!pGetSystemTime)
    pGetSystemTime = GetSystemTimeAsFileTime;

  /* Get the time and the performance counter. Get the performance counter first
     to ensure no quantum effects */
  QueryPerformanceCounter(&counter);
  pGetSystemTime(&stamp);

  now.LowPart = stamp.dwLowDateTime;
  now.HighPart = stamp.dwHighDateTime;

  /* Convert a FILETIME in 100ns ticks since 1 January 1601 to
     ns since 1 Jan 1970. */
  clock_offset.QuadPart =
    ((now.QuadPart - INT64_LITERAL(0x19DB1DED53E8000U)) * 100);

  /* Get the offset between QueryPerformanceCounter and
     GetSystemTimePreciseAsFileTime in order to return a true timestamp, rather
     than just a monotonic time source */
  clock_offset.QuadPart -= (counter.QuadPart * frequency.QuadPart);

  GetSystemTimePreciseAsFileTime(&stamp);
  now.LowPart = stamp.dwLowDateTime;
  now.HighPart = stamp.dwHighDateTime;
  now.QuadPart *= 100;
}

int64_t caml_time_counter(void)
{
  static double clock_freq = 0;
  static LARGE_INTEGER now;

  if (clock_freq == 0) {
    LARGE_INTEGER f;
    if (!QueryPerformanceFrequency(&f))
      return 0;
    clock_freq = (1000000000.0 / f.QuadPart);
  };

  if (!QueryPerformanceCounter(&now))
    return 0;
  return (int64_t)(now.QuadPart * clock_freq);
}
