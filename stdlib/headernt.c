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

#define CAML_INTERNALS

#define STRICT
#define WIN32_LEAN_AND_MEAN

#include <windows.h>
#include "caml/mlvalues.h"
#include "caml/exec.h"

#ifndef __MINGW32__
#pragma comment(linker , "/subsystem:console")
#pragma comment(lib , "kernel32")
#ifdef _UCRT
#pragma comment(lib , "ucrt.lib")
#pragma comment(lib , "vcruntime.lib")
#endif
#endif

wchar_t * default_runtime_name = RUNTIME_NAME;

static
#if _MSC_VER >= 1200
__forceinline
#else
__inline
#endif
unsigned long read_size(const char * const ptr)
{
  const unsigned char * const p = (const unsigned char * const) ptr;
  return ((unsigned long) p[0] << 24) | ((unsigned long) p[1] << 16) |
         ((unsigned long) p[2] << 8) | p[3];
}

static __inline wchar_t * read_runtime_path(HANDLE h)
{
  char buffer[TRAILER_SIZE];
  static wchar_t runtime_path[MAX_PATH];
  DWORD nread;
  int num_sections, path_size, i;
  long ofs;

  if (SetFilePointer(h, -TRAILER_SIZE, NULL, FILE_END) == -1) return NULL;
  if (! ReadFile(h, buffer, TRAILER_SIZE, &nread, NULL)) return NULL;
  if (nread != TRAILER_SIZE) return NULL;
  num_sections = read_size(buffer);
  ofs = TRAILER_SIZE + num_sections * 8;
  if (SetFilePointer(h, - ofs, NULL, FILE_END) == -1) return NULL;
  path_size = 0;
  for (i = 0; i < num_sections; i++) {
    if (! ReadFile(h, buffer, 8, &nread, NULL) || nread != 8) return NULL;
    if (buffer[0] == 'R' && buffer[1] == 'N' &&
        buffer[2] == 'T' && buffer[3] == 'M') {
      path_size = read_size(buffer + 4);
      ofs += path_size;
    } else if (path_size > 0)
      ofs += read_size(buffer + 4);
  }
  if (path_size == 0) return default_runtime_name;
  if (path_size >= MAX_PATH) return NULL;
  if (SetFilePointer(h, -ofs, NULL, FILE_END) == -1) return NULL;
  if (! ReadFile(h, runtime_path, path_size, &nread, NULL)) return NULL;
  if (nread != path_size) return NULL;
  runtime_path[path_size - 1] = 0;
  return runtime_path;
}

static BOOL WINAPI ctrl_handler(DWORD event)
{
  if (event == CTRL_C_EVENT || event == CTRL_BREAK_EVENT)
    return TRUE;                /* pretend we've handled them */
  else
    return FALSE;
}

#define msg_and_length(msg) msg , (sizeof(msg) - 1)

static __inline void __declspec(noreturn) run_runtime(wchar_t * runtime,
         wchar_t * const cmdline)
{
  wchar_t path[MAX_PATH];
  STARTUPINFO stinfo;
  PROCESS_INFORMATION procinfo;
  DWORD retcode;
  if (SearchPath(NULL, runtime, L".exe", sizeof(path)/sizeof(wchar_t), path, &runtime) == 0) {
    HANDLE errh;
    char runtime_cp[MAX_PATH];
    DWORD numwritten;
    errh = GetStdHandle(STD_ERROR_HANDLE);
    WideCharToMultiByte(CP_UTF8, 0, runtime, -1, runtime_cp, sizeof(runtime_cp), NULL, NULL);
    WriteFile(errh, msg_and_length("Cannot exec "), &numwritten, NULL);
    WriteFile(errh, msg_and_length(runtime_cp), &numwritten, NULL);
    WriteFile(errh, msg_and_length("\r\n"), &numwritten, NULL);
    ExitProcess(2);
#if _MSC_VER >= 1200
    __assume(0); /* Not reached */
#endif
  }
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
  if (!CreateProcess(path, cmdline, NULL, NULL, TRUE, 0, NULL, NULL,
                     &stinfo, &procinfo)) {
    HANDLE errh;
    char runtime_cp[MAX_PATH];
    DWORD numwritten;
    errh = GetStdHandle(STD_ERROR_HANDLE);
    WriteFile(errh, msg_and_length("Cannot exec "), &numwritten, NULL);
    WriteFile(errh, runtime_cp, strlen(runtime_cp), &numwritten, NULL);
    WriteFile(errh, msg_and_length("\r\n"), &numwritten, NULL);
    ExitProcess(2);
#if _MSC_VER >= 1200
    __assume(0); /* Not reached */
#endif
  }
  CloseHandle(procinfo.hThread);
  WaitForSingleObject(procinfo.hProcess , INFINITE);
  GetExitCodeProcess(procinfo.hProcess , &retcode);
  CloseHandle(procinfo.hProcess);
  ExitProcess(retcode);
#if _MSC_VER >= 1200
    __assume(0); /* Not reached */
#endif
}

int wmain(void)
{
  wchar_t truename[MAX_PATH];
  wchar_t * cmdline = GetCommandLine();
  wchar_t * runtime_path;
  HANDLE h;

  GetModuleFileName(NULL, truename, sizeof(truename)/sizeof(wchar_t));
  h = CreateFile(truename, GENERIC_READ, FILE_SHARE_READ | FILE_SHARE_WRITE,
                 NULL, OPEN_EXISTING, 0, NULL);
  if (h == INVALID_HANDLE_VALUE ||
      (runtime_path = read_runtime_path(h)) == NULL) {
    HANDLE errh;
    char truename_cp[MAX_PATH];
    DWORD numwritten;
    errh = GetStdHandle(STD_ERROR_HANDLE);
    WideCharToMultiByte(CP_UTF8, 0, truename, -1, truename_cp, sizeof(truename_cp), NULL, NULL);
    WriteFile(errh, msg_and_length(truename_cp), &numwritten, NULL);
    WriteFile(errh, msg_and_length(" not found or is not a bytecode"
                                   " executable file\r\n"),
              &numwritten, NULL);
    ExitProcess(2);
#if _MSC_VER >= 1200
    __assume(0); /* Not reached */
#endif
  }
  CloseHandle(h);
  run_runtime(runtime_path , cmdline);
#if _MSC_VER >= 1200
    __assume(0); /* Not reached */
#endif
#ifdef __MINGW32__
    return 0;
#endif
}
