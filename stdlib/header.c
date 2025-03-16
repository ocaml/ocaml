/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 1998 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

/* The launcher for bytecode executables (if #! is not available) */

/* C11's _Noreturn is deprecated in C23 in favour of attributes */
#if defined(__STDC_VERSION__) && __STDC_VERSION__ >= 202311L
  #define NORETURN [[noreturn]]
#else
  #define NORETURN _Noreturn
#endif

#include <errno.h>

#ifdef _WIN32

#define STRICT
#define WIN32_LEAN_AND_MEAN
#include <windows.h>

typedef wchar_t char_os;
typedef wchar_t * argv_t;
#define T(x) L ## x
#define Is_separator(c) (c == '\\' || c == '/')
#define Directory_separator_character T('\\')
#define ITOL(i) L ## #i
#define ITOT(i) ITOL(i)
#define PATH_NAME L"%Path%"

/* The header is written to be able to cope with paths greater than MAX_PATH,
   so undefine it to stop it being used in error. */
#undef MAX_PATH

#if defined(__MINGW32__) && defined(PATH_MAX)
/* mingw-w64 has a limits.h which defines PATH_MAX as an alias for MAX_PATH */
#undef PATH_MAX
#endif

#if WINDOWS_UNICODE
#define CP CP_UTF8
#else
#define CP CP_ACP
#endif

#ifndef __has_attribute
#define __has_attribute(x) 0
#endif

#if __has_attribute(fallthrough)
  #define fallthrough __attribute__ ((fallthrough))
#else
  #define fallthrough ((void) 0)
#endif

/* The maximum representable path for any API function, after internal expansion
   of \\?\ etc. is 32767 characters. PATH_MAX includes the terminator. */
#define PATH_MAX 0x8000

/* Initialised as the first statement of wmainCRTStartup */
static HANDLE hProcessHeap;

#define malloc(size) HeapAlloc(hProcessHeap, 0, (size))
#define free(memblock) HeapFree(hProcessHeap, 0, (memblock))

#define SEEK_END FILE_END

/* Initialised as the first statement of wmainCRTStartup */
static HANDLE hProcessHeap;

#define malloc(size) HeapAlloc(hProcessHeap, 0, (size))
#define free(memblock) HeapFree(hProcessHeap, 0, (memblock))

#define lseek(h, offset, origin) SetFilePointer((h), (offset), NULL, (origin))

typedef HANDLE file_descriptor;

#define unsafe_copy(dst, src, dstsize) lstrcpy(dst, src)

static int read(HANDLE h, LPVOID buffer, DWORD buffer_size)
{
  DWORD nread = 0;
  ReadFile(h, buffer, buffer_size, &nread, NULL);
  return nread;
}

static BOOL WINAPI ctrl_handler(DWORD event)
{
  if (event == CTRL_C_EVENT || event == CTRL_BREAK_EVENT)
    return TRUE;                /* pretend we've handled them */
  else
    return FALSE;
}

static int exec_file(wchar_t *file, wchar_t *cmdline, STARTUPINFO *stinfo)
{
  LPWSTR truename = (LPWSTR)malloc(PATH_MAX * sizeof(WCHAR));
  PROCESS_INFORMATION procinfo;
  DWORD retcode = ENOMEM;

  if (truename && SearchPath(NULL, file, L".exe", PATH_MAX, truename, NULL)) {
    /* Need to ignore ctrl-C and ctrl-break, otherwise we'll die and take the
       underlying OCaml program with us! */
    SetConsoleCtrlHandler(ctrl_handler, TRUE);

    if (CreateProcess(truename, cmdline, NULL, NULL, TRUE, 0, NULL, NULL,
                      stinfo, &procinfo)) {
      free(truename);
      CloseHandle(procinfo.hThread);
      WaitForSingleObject(procinfo.hProcess, INFINITE);
      GetExitCodeProcess(procinfo.hProcess, &retcode);
      CloseHandle(procinfo.hProcess);
      ExitProcess(retcode);
    } else {
      retcode = ENOEXEC;
    }
  } else {
    retcode = ENOENT;
  }

  free(truename);

  return retcode;
}

static void write_error(const wchar_t *wstr, HANDLE hOut)
{
  DWORD consoleMode, numwritten, len;
  char *str;

  if (GetConsoleMode(hOut, &consoleMode) != 0) {
    /* The output stream is a Console */
    WriteConsole(hOut, wstr, lstrlen(wstr), &numwritten, NULL);
  } else { /* The output stream is redirected */
    len = WideCharToMultiByte(CP, 0, wstr, -1, NULL, 0, NULL, NULL);
    str = (char *)malloc(len);
    WideCharToMultiByte(CP, 0, wstr, -1, str, len, NULL, NULL);
    /* len includes the terminator */
    WriteFile(hOut, str, len - 1, &numwritten, NULL);
  }
}

NORETURN static void exit_with_error(const wchar_t *wstr1,
                                     const wchar_t *wstr2,
                                     const wchar_t *wstr3)
{
  HANDLE hOut = GetStdHandle(STD_ERROR_HANDLE);
  if (wstr1) write_error(wstr1, hOut);
  if (wstr2) write_error(wstr2, hOut);
  if (wstr3) write_error(wstr3, hOut);
  write_error(L"\r\n", hOut);
  ExitProcess(2);
}

#else

#include "caml/s.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <limits.h>
#ifdef HAS_LIBGEN_H
#include <libgen.h>
#endif
#include <sys/types.h>

/* O_BINARY is defined in Gnulib, but is not POSIX */
#ifndef O_BINARY
#define O_BINARY 0
#endif

typedef int file_descriptor;

typedef char char_os;
typedef char ** argv_t;
#define T(x) x
#define Is_separator(c) (c == '/')
#define Directory_separator_character '/'
#define ITOL(x) #x
#define ITOT(x) ITOL(x)
#define PATH_NAME "$PATH"

#ifdef HAS_STRLCPY
/* The macro is named unsafe_copy because although it requires a dstsize
   argument which _may_ be passed to strlcpy, there are platforms where the
   underlying operation is unsafe and will ignore dstsize. */
#define unsafe_copy strlcpy
#else
#define unsafe_copy(dst, src, dstsize) strcpy(dst, src)
#endif

/* caml_search_in_system_path uses caml_stat_alloc and caml_executable_name also
   uses caml_stat_free */
void *caml_stat_alloc(size_t size)
{
  return malloc(size);
}

void caml_stat_free(void *ptr)
{
  free(ptr);
}

NORETURN static void exit_with_error(const char *str1,
                                     const char *str2,
                                     const char *str3)
{
  if (str1) fputs(str1, stderr);
  if (str2) fputs(str2, stderr);
  if (str3) fputs(str3, stderr);
  fputs("\n", stderr);
  exit(2);
}

static int exec_file(const char *file, char * const argv[], void *_stinfo)
{
  return (execvp(file, argv) == -1 ? errno : 0);
}

#endif /* defined(_WIN32) */

#include "caml/version.h"
#define SHORT_VERSION ITOT(OCAML_VERSION_MAJOR) T(".") ITOT(OCAML_VERSION_MINOR)

#define CAML_INTERNALS
#include "caml/exec.h"

static uint32_t read_size(const char *ptr)
{
  const unsigned char *p = (const unsigned char *)ptr;
  return ((uint32_t) p[0] << 24) | ((uint32_t) p[1] << 16) |
         ((uint32_t) p[2] << 8) | p[3];
}

static char * read_runtime_path(file_descriptor fd, uint32_t *rntm_strlen)
{
  char buffer[TRAILER_SIZE];
  char *runtime_path;
  int num_sections;
  long ofs;

  if (lseek(fd, -TRAILER_SIZE, SEEK_END) == -1) return NULL;
  if (read(fd, buffer, TRAILER_SIZE) < TRAILER_SIZE) return NULL;
  num_sections = read_size(buffer);
  ofs = TRAILER_SIZE + num_sections * 8;
  if (lseek(fd, -ofs, SEEK_END) == -1) return NULL;
  *rntm_strlen = 0;
  for (int i = 0; i < num_sections; i++) {
    if (read(fd, buffer, 8) < 8) return NULL;
    if (buffer[0] == 'R' && buffer[1] == 'N' &&
        buffer[2] == 'T' && buffer[3] == 'M') {
      *rntm_strlen = read_size(buffer + 4);
      ofs += *rntm_strlen;
    } else if (*rntm_strlen > 0)
      ofs += read_size(buffer + 4);
  }
  if (*rntm_strlen == 0) return NULL;
  if ((runtime_path = (char *)malloc(*rntm_strlen + 1)) == NULL) return NULL;
  if (lseek(fd, -ofs, SEEK_END) == -1) return NULL;
  if (read(fd, runtime_path, *rntm_strlen) != *rntm_strlen) return NULL;
  runtime_path[*rntm_strlen] = 0;
  return runtime_path;
}

/* rntm points to a buffer containing rntm_bsz characters consisting of the
   decoded content of the RNTM section (which may include NUL characters) and an
   additional NUL "terminator".
   RNTM is either <runtime>[\0] or [<runtime-dirname>]\0<runtime-basename>
   Decode rntm and search for a runtime (using argv0_dirname if non-NULL and
   required) and exec the first runtime found passing argv. */
NORETURN void search_and_exec_runtime(char_os *rntm, uint32_t rntm_bsz,
                                      argv_t argv, char_os *argv0_dirname,
                                      void *stinfo)
{
  /* rntm_end points to the NUL "terminator" of rntm (_not_ the last character
     of the RNTM section */
  const char_os *rntm_end = rntm + (rntm_bsz - 1);

  char_os *rntm_bindir_end = rntm;

  /* Scan for the first NUL character in rntm (there is always one) */
  while (*rntm_bindir_end != 0)
    rntm_bindir_end++;

  /* The first character of rntm is NUL for Enable mode */
  if (*rntm != 0) {
    /* For Disable mode, there is no NUL in RNTM, so rntm_bindir_end points to
       the terminator pointed to by rntm_end. For Fallback, there is a NUL in
       the middle of the RNTM "string", which rntm_bindir_end points at. Change
       that to a directory separator, so that rntm now points to a
       NUL-terminated full path we can attempt to exec. */
    if (rntm_bindir_end != rntm_end)
      *rntm_bindir_end = Directory_separator_character;
    int status = exec_file(rntm, argv, stinfo);
    /* exec failed. For Disable mode, there's nothing else to be tried. For
       Fallback, if the failure was for any other reason than ENOENT then there
       is also nothing else to be tried. */
    if (rntm_bindir_end == rntm_end || status != ENOENT)
      exit_with_error(T("Cannot exec "), rntm, NULL);
  }

  /* Shift rntm to point to <runtime-basename> */
  rntm = rntm_bindir_end + 1;
  if (rntm < rntm_end) {
    /* Searching takes place first in the directory containing this executable,
       if it's known. */
    if (argv0_dirname != NULL) {
      char_os *root = (char_os *)malloc((PATH_MAX + 1) * sizeof(char_os));
      if (root == NULL)
        exit_with_error(T("Out of memory"), NULL, NULL);
      unsafe_copy(root, argv0_dirname, PATH_MAX);

      /* Ensure root ends with a directory separator. root_basename points to
         the character at which to place <runtime-basename> */
      char_os *root_basename = root;
      while (*root_basename != 0)
        root_basename++;
      if (root_basename > root && !Is_separator(*(root_basename - 1)))
        *root_basename++ = Directory_separator_character;

      /* If there isn't enough space to copy rntm to root then simply skip this
         check (e.g. an executable called b.exe in a very long directory name).
         (root_basename - root) is strlen_os(root) and likewise
         (rntm_end - rntm) is strlen_os(rntm). */
      if ((rntm_end - rntm) <= PATH_MAX - (root_basename - root) - 1) {
        unsafe_copy(root_basename, rntm, PATH_MAX - (root_basename - root));
        if (exec_file(root, argv, stinfo) != ENOENT)
          exit_with_error(T("Cannot exec "), root, NULL);
      }
    }

    /* Otherwise, search in PATH */
    if (exec_file(rntm, argv, stinfo) != ENOENT)
      exit_with_error(T("Cannot exec "), rntm, NULL);
  }

  /* If we get here, we've failed... */
  exit_with_error(T("This program requires OCaml ") SHORT_VERSION T("\n")
                  T("Interpreter ("), (rntm_bindir_end + 1),
                  T(") not found alongside the program or in " PATH_NAME));
}

#ifdef _WIN32

#undef RtlMoveMemory
void __declspec(dllimport) __stdcall RtlMoveMemory(void *Destination,
                                                   const void *Source,
                                                   size_t Length);

NORETURN void __cdecl wmainCRTStartup(void)
{
  LPWSTR truename;
  LPWSTR dirname;
  uint32_t rntm_strlen = 0, rntm_bsz = 0;
  char *runtime_path;
  wchar_t *wruntime_path, *basename;
  HANDLE h;

  hProcessHeap = GetProcessHeap();

  truename = (LPWSTR)malloc(PATH_MAX * sizeof(WCHAR));
  dirname = (LPWSTR)malloc(PATH_MAX * sizeof(WCHAR));

  if (truename == NULL || dirname == NULL
      || GetModuleFileName(NULL, truename, PATH_MAX) == 0
      || GetFullPathName(truename, PATH_MAX, dirname, &basename) >= PATH_MAX)
    exit_with_error(L"Out of memory", NULL, NULL);
  /* GetFullPathName leaves basename pointing to the first character of the
     basename, so setting that to NUL means the string pointed to by dirname
     is the dirname of the currently running executable with a trailing
     separator (although search_and_exec_runtime will check that anyway) */
  *basename = 0;

  /* Mark the HANDLE as inheritable so ocamlrun can use it */
  SECURITY_ATTRIBUTES sa;
  sa.nLength = sizeof(sa);
  sa.lpSecurityDescriptor = NULL;
  sa.bInheritHandle = TRUE;
  h = CreateFile(truename, GENERIC_READ, FILE_SHARE_READ | FILE_SHARE_WRITE,
                 &sa, OPEN_EXISTING, 0, NULL);
  if (h == INVALID_HANDLE_VALUE
      || (runtime_path = read_runtime_path(h, &rntm_strlen)) == NULL
      || (wruntime_path =
            (wchar_t *)malloc((rntm_strlen + 1) * sizeof(wchar_t))) == NULL
      || (rntm_bsz = MultiByteToWideChar(CP, 0, runtime_path, rntm_strlen + 1,
                                         wruntime_path, rntm_strlen + 1)) == 0)
    exit_with_error(NULL, truename,
                    L" not found or is not a bytecode executable file");
  free(runtime_path);
  free(truename);
  STARTUPINFO stinfo;
  /* Retrieve the existing STARTUPINFO structure - however this header was
     invoked is morally how we should invoke ocamlrun, but we also need to
     set-up or augment the cbReserved2 / lpReserved2 members in order to pass
     the HANDLE h to ocamlrun as a CRT fd. The cloexec.ml test checks that
     existing fds are passed through successfully. The use of lpReserved2 by the
     CRT can be seen in the Universal CRT sources info exec/spawnv.cpp for the
     code which sets the buffer up and in lowio/ioinit.cpp which reads the
     buffer provided to the process. The semantics of this buffer are unchanged
     since the very beginning of Windows NT.
     It is a relatively well-documented "trick" to be able to pass up to 64KiB
     of information to a new process using lpReserved2, on condition that the
     data respects the CRT's requirements. The CRT processes lpReserved2 if it
     is not NULL and if cbReserved2 is non-zero - it performs no further
     checking beyond that. Applications can therefore embed additional data by
     setting cbReserved2 to the actual size of lpReserved2 and simply ensuring
     that the first 4 bytes pointed to by lpReserved2 are zero.
     Cygwin uses this mechanism when invoking processes to allow the Cygwin DLL
     to pick up the required information about the caller, amongst other things
     to implement fork (it's also used as part of argument passing).
     The code below must therefore cater for three cases:
     1. cbReserved2 == 0 / lpReserved2 == NULL, in which case the structure must
        be created
     2. cbReserved2 > 0 but there are fewer than 3 fds in the structure, in
        which case empty handles must be added so that our HANDLE is fd 3
     3. cbReserved2 > 0 and there are already 3 or more fds in the structure, in
        which case our HANDLE is appended to the end of the structure */
  GetStartupInfo(&stinfo);

  /* This header avoids the CRT to keep its size down - the Windows API doesn't
     have anything sprintf-like, however, the largest fd-number fits comfortably
     within a 16-bit wide character and we know that it will never be zero - the
     number of the fd is therefore passed to ocamlrun as a single wide-character
     string where the code-point represents the fd.
     Nemo nunc te poteste servare. */
  WCHAR fd[2] = {0, 0};

  /* Match the CRT's check - ignore the existing values if either cbReserved2 is
     zero _or_ lpReserved2 is NULL */
  if (stinfo.cbReserved2 > 0 && stinfo.lpReserved2 == NULL)
    stinfo.cbReserved2 = 0;

  int existing_count = 0;
  /* Work out the fd number for h */
  if (stinfo.cbReserved2 > 0) {
    existing_count = *(int *)stinfo.lpReserved2;
    fd[0] = existing_count;
    /* If there is a structure present, but it has no fds, discard it. */
    if (existing_count == 0)
      stinfo.cbReserved2 = 0;
  }
  /* Allow for the standard handles */
  if (fd[0] < 3)
    fd[0] = 3;

  WORD buffer_size = sizeof(int) + (fd[0] + 1) * (1 + sizeof(HANDLE));
  LPBYTE buffer = (LPBYTE)malloc(buffer_size);

  /* Store the total number of handles */
  *(int *)buffer = fd[0] + 1;

  /* Copy the existing flags and HANDLEs */
  if (stinfo.cbReserved2 > 0) {
    RtlMoveMemory(buffer + sizeof(int), stinfo.lpReserved2 + sizeof(int),
                  existing_count);
    RtlMoveMemory(buffer + sizeof(int) + fd[0] + 1,
                  stinfo.lpReserved2 + sizeof(int) + existing_count,
                  existing_count * sizeof(HANDLE));
  }

  /* Pointers to the next slot for flags and the next slot for a HANDLE */
  LPBYTE osflags =
    buffer + sizeof(int) + existing_count;
  LPHANDLE oshandles =
    (LPHANDLE)(buffer + sizeof(int) + fd[0] + 1
               + existing_count * sizeof(HANDLE));

  /* Ensure the standard fds are populated. Unrolled to prevent cl requiring the
     memset intrinsic. */
  switch (existing_count) {
    case 0:
      *osflags++ = 0;
      *oshandles++ = INVALID_HANDLE_VALUE;
      fallthrough;
    case 1:
      *osflags++ = 0;
      *oshandles++ = INVALID_HANDLE_VALUE;
      fallthrough;
    case 2:
      *osflags++ = 0;
      *oshandles++ = INVALID_HANDLE_VALUE;
  }

  /* Add h to the structure */
  *osflags = 1;
  *oshandles = h;

  stinfo.cbReserved2 = buffer_size;
  stinfo.lpReserved2 = buffer;

  SetEnvironmentVariable(L"__OCAML_EXEC_FD", fd);
  search_and_exec_runtime(wruntime_path, rntm_bsz,
                          GetCommandLine(), dirname, &stinfo);
}

#else

/* Borrowed from libcamlrun */
char * caml_search_in_system_path(const char *);
char * caml_executable_name(void);

int main(int argc, char *argv[])
{
  char *truename, *runtime_path, *argv0_dirname;
  uint32_t rntm_strlen = 0;
  int fd;

  if (argc < 1)
    exit_with_error("Unable to load bytecode image", NULL, NULL);

  truename = caml_executable_name();
  if (truename == NULL) truename = caml_search_in_system_path(argv[0]);
  if (truename == NULL) truename = argv[0];
  fd = open(truename, O_RDONLY | O_BINARY);
  if (fd == -1 || (runtime_path = read_runtime_path(fd, &rntm_strlen)) == NULL)
    exit_with_error(NULL, truename,
                    " not found or is not a bytecode executable file");

  size_t truename_len = strlen(truename);
  char *value = (char *)malloc(10 + 1 + truename_len + 1);
  snprintf(value, 11, "%u,", fd);
  strcat(value, truename);
#ifdef HAS_SETENV_UNSETENV
  setenv("__OCAML_EXEC_FD", value, 1);
#else
#error "Require a way to set environment variables"
#endif

#ifdef HAS_LIBGEN_H
  argv0_dirname = dirname(strdup(truename));
#else
  argv0_dirname = NULL;
#endif

  /* read_runtime_path returns the actual size of RNTM, but the buffer returned
     is guaranteed to have a null character following the final character of
     RNTM. */
  search_and_exec_runtime(runtime_path, rntm_strlen + 1, argv, argv0_dirname,
                          NULL);
}

#endif /* defined(_WIN32) */
