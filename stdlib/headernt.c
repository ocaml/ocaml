/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1998 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License.         */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#define STRICT
#define WIN32_LEAN_AND_MEAN

#include <windows.h>
#include "../byterun/exec.h"

#pragma comment(linker , "/entry:headerentry")
#pragma comment(linker , "/subsystem:console")
#pragma comment(lib , "kernel32")

char * default_runtime_name = "ocamlrun";

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

static __inline char * read_runtime_path(HANDLE h)
{
  char buffer[TRAILER_SIZE];
  static char runtime_path[MAX_PATH];
  DWORD nread;
  struct exec_trailer tr;
  long size;

  if (SetFilePointer(h, -TRAILER_SIZE, NULL, FILE_END) == -1) return NULL;
  if (! ReadFile(h, buffer, TRAILER_SIZE, &nread, NULL)) return NULL;
  if (nread != TRAILER_SIZE) return NULL;
  tr.path_size = read_size(buffer);
  tr.code_size = read_size(buffer + 4);
  tr.prim_size = read_size(buffer + 8);
  tr.data_size = read_size(buffer + 12);
  tr.symbol_size = read_size(buffer + 16);
  tr.debug_size = read_size(buffer + 20);
  if (tr.path_size >= MAX_PATH) return NULL;
  if (tr.path_size == 0) return default_runtime_name;
  size = tr.path_size + tr.code_size + tr.prim_size +
         tr.data_size + tr.symbol_size + tr.debug_size + TRAILER_SIZE;
  if (SetFilePointer(h, -size, NULL, FILE_END) == -1) return NULL;
  if (! ReadFile(h, runtime_path, tr.path_size, &nread, NULL)) return NULL;
  if (nread != tr.path_size) return NULL;
  runtime_path[tr.path_size - 1] = 0;
  return runtime_path;
}

#define msg_and_length(msg) msg , (sizeof(msg) - 1)

static __inline void __declspec(noreturn) run_runtime(char * runtime,
         char * const cmdline)
{
  char path[MAX_PATH];
  STARTUPINFO stinfo;
  PROCESS_INFORMATION procinfo;
  DWORD retcode;
  if (SearchPath(NULL, runtime, ".exe", MAX_PATH, path, &runtime) == 0) {
    HANDLE errh;
    DWORD numwritten;
    errh = GetStdHandle(STD_ERROR_HANDLE);
    WriteFile(errh, msg_and_length("Cannot exec "), &numwritten, NULL);
    WriteFile(errh, runtime, strlen(runtime), &numwritten, NULL);
    WriteFile(errh, msg_and_length("\r\n"), &numwritten, NULL);
    ExitProcess(2);
#if _MSC_VER >= 1200
    __assume(0); /* Not reached */
#endif
  }
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
    DWORD numwritten;
    errh = GetStdHandle(STD_ERROR_HANDLE);
    WriteFile(errh, msg_and_length("Cannot exec "), &numwritten, NULL);
    WriteFile(errh, runtime, strlen(runtime), &numwritten, NULL);
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

void __declspec(noreturn) __cdecl headerentry()
{
  char truename[MAX_PATH];
  char * cmdline = GetCommandLine();
  char * runtime_path;
  HANDLE h;

  GetModuleFileName(NULL, truename, sizeof(truename));
  h = CreateFile(truename, GENERIC_READ, FILE_SHARE_READ | FILE_SHARE_WRITE,
                 NULL, OPEN_EXISTING, 0, NULL);
  if (h == INVALID_HANDLE_VALUE ||
      (runtime_path = read_runtime_path(h)) == NULL) {
    HANDLE errh;
    DWORD numwritten;
    errh = GetStdHandle(STD_ERROR_HANDLE);
    WriteFile(errh, truename, strlen(truename), &numwritten, NULL);
    WriteFile(errh, msg_and_length(" not found or is not a bytecode executable file\r\n"),
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
}
