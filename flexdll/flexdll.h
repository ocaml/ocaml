/*****************************************************************
   FlexDLL
   Alain Frisch

   Copyright 2007 Institut National de Recherche en Informatique et
   en Automatique.

******************************************************************/

/* Header for the runtime support library */

#ifndef FLEXDLL_H
#define FLEXDLL_H

#include <wchar.h>

#define FLEXDLL_RTLD_GLOBAL 0x0001
#define FLEXDLL_RTLD_LOCAL  0x0000
#define FLEXDLL_RTLD_NOEXEC 0x0002

#ifdef __cplusplus
extern "C"
{
#endif

void *flexdll_dlopen(const char *, int);
#ifndef CYGWIN
void *flexdll_wdlopen(const wchar_t *, int);
#endif

void *flexdll_dlsym(void *, const char *);
void flexdll_dlclose(void *);
char *flexdll_dlerror(void);

void flexdll_dump_exports(void *);
void flexdll_dump_relocations(void *);

#ifdef __cplusplus
}
#endif

#endif
