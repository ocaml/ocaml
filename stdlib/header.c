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

#if WINDOWS_UNICODE
#define CP CP_UTF8
/* The characters in RNTM will be converted from UTF-8 to UTF-16. Parasitically,
   there could be 4 bytes in RNTM for every wchar_t in the actual value. */
#define RNTM_ENCODING_LENGTH 4
#else
#define CP CP_ACP
#endif

/* mingw-w64 has a limits.h which defines PATH_MAX as an alias for MAX_PATH */
#if !defined(PATH_MAX)
#define PATH_MAX MAX_PATH
#endif

#define SEEK_END FILE_END

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

static int exec_file(wchar_t *file, wchar_t *cmdline)
{
  wchar_t truename[MAX_PATH];
  STARTUPINFO stinfo;
  PROCESS_INFORMATION procinfo;
  DWORD retcode;

  if (SearchPath(NULL, file, L".exe", sizeof(truename)/sizeof(wchar_t),
                 truename, NULL)) {
    /* Need to ignore ctrl-C and ctrl-break, otherwise we'll die and take the
       underlying OCaml program with us! */
    SetConsoleCtrlHandler(ctrl_handler, TRUE);

    stinfo.cb = sizeof(stinfo);
    stinfo.lpReserved = NULL;
    stinfo.lpDesktop = NULL;
    stinfo.lpTitle = NULL;
    stinfo.dwFlags = 0;
    stinfo.cbReserved2 = 0;
    stinfo.lpReserved2 = NULL;
    if (CreateProcess(truename, cmdline, NULL, NULL, TRUE, 0, NULL, NULL,
                      &stinfo, &procinfo)) {
      CloseHandle(procinfo.hThread);
      WaitForSingleObject(procinfo.hProcess, INFINITE);
      GetExitCodeProcess(procinfo.hProcess, &retcode);
      CloseHandle(procinfo.hProcess);
      ExitProcess(retcode);
    } else {
      return ENOEXEC;
    }
  } else {
    return ENOENT;
  }
}

static void write_error(const wchar_t *wstr, HANDLE hOut)
{
  DWORD consoleMode, numwritten, len;
  char str[MAX_PATH];

  if (GetConsoleMode(hOut, &consoleMode) != 0) {
    /* The output stream is a Console */
    WriteConsole(hOut, wstr, lstrlen(wstr), &numwritten, NULL);
  } else { /* The output stream is redirected */
    len =
      WideCharToMultiByte(CP, 0, wstr, lstrlen(wstr), str, sizeof(str),
                          NULL, NULL);
    WriteFile(hOut, str, len, &numwritten, NULL);
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
#include <sys/stat.h>

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

#ifndef __CYGWIN__

/* Normal Unix search path function */

static char * searchpath(char * name)
{
  static char fullname[PATH_MAX + 1];
  char * path;
  struct stat st;

  for (char *p = name; *p != 0; p++) {
    if (*p == '/') return name;
  }
  path = getenv("PATH");
  if (path == NULL) return name;
  while(1) {
    char * p;
    for (p = fullname; *path != 0 && *path != ':'; p++, path++)
      if (p < fullname + PATH_MAX) *p = *path;
    if (p != fullname && p < fullname + PATH_MAX)
      *p++ = '/';
    for (char *q = name; *q != 0; p++, q++)
      if (p < fullname + PATH_MAX) *p = *q;
    *p = 0;
    if (stat(fullname, &st) == 0 && S_ISREG(st.st_mode)) break;
    if (*path == 0) return name;
    path++;
  }
  return fullname;
}

#else

/* Special version for Cygwin32: takes care of the ".exe" implicit suffix */

static int file_ok(char * name)
{
  int fd;
  /* Cannot use stat() here because it adds ".exe" implicitly */
  fd = open(name, O_RDONLY);
  if (fd == -1) return 0;
  close(fd);
  return 1;
}

static char * searchpath(char * name)
{
  char * path, * fullname;

  path = getenv("PATH");
  fullname = malloc(strlen(name) + (path == NULL ? 0 : strlen(path)) + 6);
  /* 6 = "/" plus ".exe" plus final "\0" */
  if (fullname == NULL) return name;
  /* Check for absolute path name */
  for (char *p = name; *p != 0; p++) {
    if (*p == '/' || *p == '\\') {
      if (file_ok(name)) return name;
      strcpy(fullname, name);
      strcat(fullname, ".exe");
      if (file_ok(fullname)) return fullname;
      return name;
    }
  }
  /* Search in path */
  if (path == NULL) return name;
  while(1) {
    char * p;
    for (p = fullname; *path != 0 && *path != ':'; p++, path++) *p = *path;
    if (p != fullname) *p++ = '/';
    strcpy(p, name);
    if (file_ok(fullname)) return fullname;
    strcat(fullname, ".exe");
    if (file_ok(fullname)) return fullname;
    if (*path == 0) break;
    path++;
  }
  return name;
}

#endif

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

static int exec_file(const char *file, char * const argv[])
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

#ifndef RNTM_ENCODING_LENGTH
#define RNTM_ENCODING_LENGTH 1
#endif

static char * read_runtime_path(file_descriptor fd, uint32_t *rntm_strlen)
{
  char buffer[TRAILER_SIZE];
  static char runtime_path[PATH_MAX * RNTM_ENCODING_LENGTH];
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
  /* The last character of runtime_path must be '\0', so RNTM must be strictly
     less than PATH_MAX */
  if (*rntm_strlen >= PATH_MAX * RNTM_ENCODING_LENGTH) return NULL;
  if (lseek(fd, -ofs, SEEK_END) == -1) return NULL;
  if (read(fd, runtime_path, *rntm_strlen) != *rntm_strlen) return NULL;

  return runtime_path;
}

/* rntm points to a buffer containing rntm_bsz characters consisting of the
   decoded content of the RNTM section (which may include NUL characters) and an
   additional NUL "terminator".
   RNTM is either <runtime>[\0] or [<runtime-dirname>]\0<runtime-basename>
   Decode rntm and search for a runtime (using argv0_dirname if non-NULL and
   required) and exec the first runtime found passing argv. */
NORETURN void search_and_exec_runtime(char_os *rntm, uint32_t rntm_bsz,
                                      argv_t argv, char_os *argv0_dirname)
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
    int status = exec_file(rntm, argv);
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
      char_os root[PATH_MAX];
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
        if (exec_file(root, argv) != ENOENT)
          exit_with_error(T("Cannot exec "), root, NULL);
      }
    }

    /* Otherwise, search in PATH */
    if (exec_file(rntm, argv) != ENOENT)
      exit_with_error(T("Cannot exec "), rntm, NULL);
  }

  /* If we get here, we've failed... */
  exit_with_error(T("This program requires OCaml ") SHORT_VERSION T("\n")
                  T("Interpreter ("), (rntm_bindir_end + 1),
                  T(") not found alongside the program or in " PATH_NAME));
}

#ifdef _WIN32

NORETURN void __cdecl wmainCRTStartup(void)
{
  wchar_t module[MAX_PATH];
  wchar_t truename[MAX_PATH];
  uint32_t rntm_strlen = 0, rntm_bsz = 0;
  char *runtime_path;
  wchar_t wruntime_path[MAX_PATH], *dirname;
  HANDLE h;

  if (GetModuleFileName(NULL, module, sizeof(module)/sizeof(wchar_t)) == 0)
    exit_with_error(L"Out of memory", NULL, NULL);

  h = CreateFile(module, GENERIC_READ, FILE_SHARE_READ | FILE_SHARE_WRITE,
                 NULL, OPEN_EXISTING, 0, NULL);


  /* read_runtime_path returns the actual size of RNTM, but the buffer returned
     is guaranteed to have a null character following the final character of
     RNTM. */
  if (h == INVALID_HANDLE_VALUE
      || (runtime_path = read_runtime_path(h, &rntm_strlen)) == NULL
      || (rntm_bsz =
            MultiByteToWideChar(CP, 0, runtime_path, rntm_strlen + 1,
                                wruntime_path,
                                sizeof(wruntime_path)/sizeof(wchar_t))) == 0
      || GetFullPathName(module, sizeof(truename)/sizeof(wchar_t), truename,
                         &dirname) >= sizeof(truename)/sizeof(wchar_t))
    exit_with_error(NULL, truename,
                    L" not found or is not a bytecode executable file");
  CloseHandle(h);

  if (dirname) {
    /* GetFullPathName leaves dirname pointing to the first character of the
       basename, so setting that to NUL means the string pointed to by truename
       is the dirname of the currently running executable with a trailing
       separator (although search_and_exec_runtime will check that anyway) */
    *dirname = 0;
    dirname = truename;
  }

  search_and_exec_runtime(wruntime_path, rntm_bsz, GetCommandLine(), dirname);
}

#else

int main(int argc, char *argv[])
{
  char *truename, *runtime_path, *argv0_dirname;
  uint32_t rntm_strlen = 0;
  int fd;

  truename = searchpath(argv[0]);
  fd = open(truename, O_RDONLY | O_BINARY);
  if (fd == -1 || (runtime_path = read_runtime_path(fd, &rntm_strlen)) == NULL)
    exit_with_error(NULL, truename,
                    " not found or is not a bytecode executable file");
  close(fd);

#ifdef HAS_LIBGEN_H
  argv0_dirname = dirname(strdup(truename));
#else
  argv0_dirname = NULL;
#endif

  argv[0] = truename;
  /* read_runtime_path returns the actual size of RNTM, but the buffer returned
     is guaranteed to have a null character following the final character of
     RNTM. */
  search_and_exec_runtime(runtime_path, rntm_strlen + 1, argv, argv0_dirname);
}

#endif /* defined(_WIN32) */
