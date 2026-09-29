/*****************************************************************
   FlexDLL
   Alain Frisch

   Copyright 2007 Institut National de Recherche en Informatique et
   en Automatique.

******************************************************************/

/* Custom entry point to perform relocations before the real
   entry point is called */

/* The adress of the flexdll_relocate function is passed in an
   environment variable. This is ugly, but I couldn't find a cleaner
   solution. Let me know if you have some idea! */

#include <stdlib.h>
#include <stdio.h>
#include <windows.h>

extern int reloctbl;

static int flexdll_init(void) {
  int (*sym)(void *) = 0;
  char *s = getenv("FLEXDLL_RELOCATE");
  if (!s) { fprintf(stderr, "Cannot find FLEXDLL_RELOCATE\n"); return FALSE; }
  sscanf(s,"%p",(void **) &sym);
  /* sym = 0 means "loaded not for execution" */
  if (!sym || sym(&reloctbl)) return TRUE;
  return FALSE;
}

#ifdef __GNUC__
#ifdef __CYGWIN__
#define entry  _cygwin_dll_entry
#endif
#ifdef __MINGW32__
#define entry DllMainCRTStartup
#endif
#else
#define entry _DllMainCRTStartup
#endif


BOOL WINAPI entry(HINSTANCE, DWORD, LPVOID);

BOOL WINAPI FlexDLLiniter(HINSTANCE hinstDLL, DWORD fdwReason,
			  LPVOID lpReserved) {
  if (fdwReason == DLL_PROCESS_ATTACH && !flexdll_init())
    return FALSE;

  return entry(hinstDLL, fdwReason, lpReserved);
}
