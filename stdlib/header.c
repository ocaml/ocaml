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

#ifdef _WIN32

#define STRICT
#define WIN32_LEAN_AND_MEAN
#include <windows.h>

#if WINDOWS_UNICODE
#define CP CP_UTF8
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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <limits.h>
#include <sys/types.h>
#include <sys/stat.h>

/* O_BINARY is defined in Gnulib, but is not POSIX */
#ifndef O_BINARY
#define O_BINARY 0
#endif

typedef int file_descriptor;

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

#endif /* defined(_WIN32) */

#define CAML_INTERNALS
#include "caml/exec.h"

static uint32_t read_size(const char *ptr)
{
  const unsigned char *p = (const unsigned char *)ptr;
  return ((uint32_t) p[0] << 24) | ((uint32_t) p[1] << 16) |
         ((uint32_t) p[2] << 8) | p[3];
}

static char * read_runtime_path(file_descriptor fd)
{
  char buffer[TRAILER_SIZE];
  static char runtime_path[PATH_MAX];
  int num_sections;
  uint32_t path_size;
  long ofs;

  if (lseek(fd, -TRAILER_SIZE, SEEK_END) == -1) return NULL;
  if (read(fd, buffer, TRAILER_SIZE) < TRAILER_SIZE) return NULL;
  num_sections = read_size(buffer);
  ofs = TRAILER_SIZE + num_sections * 8;
  if (lseek(fd, -ofs, SEEK_END) == -1) return NULL;
  path_size = 0;
  for (int i = 0; i < num_sections; i++) {
    if (read(fd, buffer, 8) < 8) return NULL;
    if (buffer[0] == 'R' && buffer[1] == 'N' &&
        buffer[2] == 'T' && buffer[3] == 'M') {
      path_size = read_size(buffer + 4);
      ofs += path_size;
    } else if (path_size > 0)
      ofs += read_size(buffer + 4);
  }
  if (path_size == 0) return NULL;
  if (path_size >= PATH_MAX) return NULL;
  if (lseek(fd, -ofs, SEEK_END) == -1) return NULL;
  if (read(fd, runtime_path, path_size) != path_size) return NULL;
  return runtime_path;
}

#ifdef _WIN32

NORETURN void __cdecl wmainCRTStartup(void)
{
  wchar_t truename[MAX_PATH];
  char *runtime_path;
  wchar_t wruntime_path[MAX_PATH];
  HANDLE h;
  STARTUPINFO stinfo;
  PROCESS_INFORMATION procinfo;
  DWORD retcode;

  if (GetModuleFileName(NULL, truename, sizeof(truename)/sizeof(wchar_t)) == 0)
    exit_with_error(L"Out of memory", NULL, NULL);

  h = CreateFile(truename, GENERIC_READ, FILE_SHARE_READ | FILE_SHARE_WRITE,
                 NULL, OPEN_EXISTING, 0, NULL);
  if (h == INVALID_HANDLE_VALUE ||
      (runtime_path = read_runtime_path(h)) == NULL ||
      !MultiByteToWideChar(CP, 0, runtime_path, -1, wruntime_path,
                           sizeof(wruntime_path)/sizeof(wchar_t)))
    exit_with_error(NULL, truename,
                    L" not found or is not a bytecode executable file");
  CloseHandle(h);
  if (SearchPath(NULL, wruntime_path, L".exe", sizeof(truename)/sizeof(wchar_t),
                 truename, NULL)) {
    /* Need to ignore ctrl-C and ctrl-break, otherwise we'll die and take
       the underlying OCaml program with us! */
    SetConsoleCtrlHandler(ctrl_handler, TRUE);

    stinfo.cb = sizeof(stinfo);
    stinfo.lpReserved = NULL;
    stinfo.lpDesktop = NULL;
    stinfo.lpTitle = NULL;
    stinfo.dwFlags = 0;
    stinfo.cbReserved2 = 0;
    stinfo.lpReserved2 = NULL;
    if (CreateProcess(truename, GetCommandLine(), NULL, NULL, TRUE, 0,
                      NULL, NULL, &stinfo, &procinfo)) {
      CloseHandle(procinfo.hThread);
      WaitForSingleObject(procinfo.hProcess, INFINITE);
      GetExitCodeProcess(procinfo.hProcess, &retcode);
      CloseHandle(procinfo.hProcess);
      ExitProcess(retcode);
    }
  }

  exit_with_error(L"Cannot exec ", wruntime_path, NULL);
}

#else

int main(int argc, char *argv[])
{
  char *truename, *runtime_path;
  int fd;

  truename = searchpath(argv[0]);
  fd = open(truename, O_RDONLY | O_BINARY);
  if (fd == -1 || (runtime_path = read_runtime_path(fd)) == NULL)
    exit_with_error(NULL, truename,
                    " not found or is not a bytecode executable file");
  close(fd);

  argv[0] = truename;
  execvp(runtime_path, argv);

  exit_with_error("Cannot exec ", runtime_path, NULL);
}

#endif /* defined(_WIN32) */
