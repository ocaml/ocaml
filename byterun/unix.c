/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*           Xavier Leroy, projet Cristal, INRIA Rocquencourt          */
/*                                                                     */
/*  Copyright 2001 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

/* Unix-specific stuff */

#define _GNU_SOURCE
           /* Helps finding RTLD_DEFAULT in glibc */

#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include "config.h"
#ifdef SUPPORT_DYNAMIC_LINKING
#ifdef HAS_NSLINKMODULE
#include <mach-o/dyld.h>
#elif defined(__CYGWIN32__)
#include "flexdll.h"
#else
#include <dlfcn.h>
#endif
#endif
#ifdef HAS_UNISTD
#include <unistd.h>
#endif
#ifdef HAS_DIRENT
#include <dirent.h>
#else
#include <sys/dir.h>
#endif
#include "memory.h"
#include "misc.h"
#include "osdeps.h"

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
    for (n = 0; q[n] != 0 && q[n] != ':'; n++) /*nothing*/;
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
    if (*p == '/') goto not_found;
  }
  for (i = 0; i < path->size; i++) {
    fullname = caml_stat_alloc(strlen((char *)(path->contents[i])) +
                               strlen(name) + 2);
    strcpy(fullname, (char *)(path->contents[i]));
    if (fullname[0] != 0) strcat(fullname, "/");
    strcat(fullname, name);
    if (stat(fullname, &st) == 0 && S_ISREG(st.st_mode)) return fullname;
    caml_stat_free(fullname);
  }
 not_found:
  fullname = caml_stat_alloc(strlen(name) + 1);
  strcpy(fullname, name);
  return fullname;
}
  
#ifdef __CYGWIN32__

/* Cygwin needs special treatment because of the implicit ".exe" at the
   end of executable file names */

static int cygwin_file_exists(char * name)
{
  int fd;
  /* Cannot use stat() here because it adds ".exe" implicitly */
  fd = open(name, O_RDONLY);
  if (fd == -1) return 0;
  close(fd);
  return 1;
}

static char * cygwin_search_exe_in_path(struct ext_table * path, char * name)
{
  char * p, * fullname;
  int i;

  for (p = name; *p != 0; p++) {
    if (*p == '/' || *p == '\\') goto not_found;
  }
  for (i = 0; i < path->size; i++) {
    fullname = caml_stat_alloc(strlen((char *)(path->contents[i])) +
                               strlen(name) + 6);
    strcpy(fullname, (char *)(path->contents[i]));
    strcat(fullname, "/");
    strcat(fullname, name);
    if (cygwin_file_exists(fullname)) return fullname;
    strcat(fullname, ".exe");
    if (cygwin_file_exists(fullname)) return fullname;
    caml_stat_free(fullname);
  }
 not_found:
  fullname = caml_stat_alloc(strlen(name) + 5);
  strcpy(fullname, name);
  if (cygwin_file_exists(fullname)) return fullname;
  strcat(fullname, ".exe");
  if (cygwin_file_exists(fullname)) return fullname;
  strcpy(fullname, name);
  return fullname;
}
  
#endif

char * caml_search_exe_in_path(char * name)
{
  struct ext_table path;
  char * tofree;
  char * res;

  caml_ext_table_init(&path, 8);
  tofree = caml_decompose_path(&path, getenv("PATH"));
#ifndef __CYGWIN32__
  res = caml_search_in_path(&path, name);
#else
  res = cygwin_search_exe_in_path(&path, name);
#endif
  caml_stat_free(tofree);
  caml_ext_table_free(&path, 0);
  return res;
}

char * caml_search_dll_in_path(struct ext_table * path, char * name)
{
  char * dllname = caml_stat_alloc(strlen(name) + 4);
  char * res;
  strcpy(dllname, name);
  strcat(dllname, ".so");
  res = caml_search_in_path(path, dllname);
  caml_stat_free(dllname);
  return res;
}

#ifdef SUPPORT_DYNAMIC_LINKING
#ifdef HAS_NSLINKMODULE
/* Use MacOSX bundles */

static char *dlerror_string = "No error";

/* Need to emulate dlopen behaviour by caching open libraries */
typedef struct bundle_entry {
  struct bundle_entry *next;
  char *name;
  void *handle;
  int count;
} entry_t;

entry_t bundle_list = {NULL,NULL,NULL,0};

entry_t *caml_lookup_bundle(const char *name)
{
  entry_t *current = bundle_list.next, *last = &bundle_list;

  while (current !=NULL) {
    if (!strcmp(name,current->name))
      return current;
    last = current;
    current = current->next;
  }
  current = (entry_t*) malloc(sizeof(entry_t)+strlen(name)+1);
  current->name = (char*)(current+1);
  strcpy(current->name, name);
  current->count = 0;
  current->next = NULL;
  last->next = current;
  return current;
}

void * caml_dlopen(char * libname, int for_execution)
{
  NSObjectFileImage image;
  entry_t *bentry = caml_lookup_bundle(libname);
  NSObjectFileImageReturnCode retCode;
  void *result = NULL;

  if (bentry->count > 0)
    return bentry->handle;

  retCode = NSCreateObjectFileImageFromFile(libname, &image);
  switch (retCode) {
  case NSObjectFileImageSuccess:
    dlerror_string = NULL;
    result = (void*)NSLinkModule(image, libname, NSLINKMODULE_OPTION_BINDNOW
                                 | NSLINKMODULE_OPTION_RETURN_ON_ERROR);
    if (result != NULL) {
      bentry->count++;
      bentry->handle = result;
    }
    else NSDestroyObjectFileImage(image);
    break;
  case NSObjectFileImageAccess:
    dlerror_string = "cannot access this bundle"; break;
  case NSObjectFileImageArch:
    dlerror_string = "this bundle has wrong CPU architecture"; break;
  case NSObjectFileImageFormat:
  case NSObjectFileImageInappropriateFile:
    dlerror_string = "this file is not a proper bundle"; break;
  default:
    dlerror_string = "could not read object file"; break;
  }
  return result;
}

void caml_dlclose(void * handle)
{
  entry_t *current = bundle_list.next;
  int close = 1;
  
  dlerror_string = NULL;
  while (current != NULL) {
    if (current->handle == handle) {
      current->count--;
      close = (current->count == 0);
      break;
    }
    current = current->next;
  }
  if (close)
    NSUnLinkModule((NSModule)handle, NSUNLINKMODULE_OPTION_NONE);
}

void * caml_dlsym(void * handle, char * name)
{
  NSSymbol sym;
  char _name[1000] = "_";
  strncat (_name, name, 998);
  dlerror_string = NULL;
  sym = NSLookupSymbolInModule((NSModule)handle, _name);
  if (sym != NULL) return NSAddressOfSymbol(sym);
  else return NULL;
}

void * caml_globalsym(char * name)
{
  return NULL;
}

char * caml_dlerror(void)
{
  NSLinkEditErrors c;
  int errnum;
  const char *fileName, *errorString;
  if (dlerror_string != NULL) return dlerror_string;
  NSLinkEditError(&c,&errnum,&fileName,&errorString);
  return (char *) errorString;
}

#elif defined(__CYGWIN32__)
/* Use flexdll */

void * caml_dlopen(char * libname, int for_execution)
{
  int flags = FLEXDLL_RTLD_GLOBAL;
  if (!for_execution) flags |= FLEXDLL_RTLD_NOEXEC;
  return flexdll_dlopen(libname, flags);
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
/* Use normal dlopen */

#ifndef RTLD_GLOBAL
#define RTLD_GLOBAL 0
#endif
#ifndef RTLD_NODELETE
#define RTLD_NODELETE 0
#endif

void * caml_dlopen(char * libname, int for_execution)
{
  return dlopen(libname, RTLD_NOW | RTLD_GLOBAL | RTLD_NODELETE);
  /* Could use RTLD_LAZY if for_execution == 0, but needs testing */
}

void caml_dlclose(void * handle)
{
  dlclose(handle);
}

void * caml_dlsym(void * handle, char * name)
{
#ifdef DL_NEEDS_UNDERSCORE
  char _name[1000] = "_";
  strncat (_name, name, 998);
  name = _name;
#endif
  return dlsym(handle, name);
}

void * caml_globalsym(char * name)
{
#ifdef RTLD_DEFAULT
  return caml_dlsym(RTLD_DEFAULT, name);
#else
  return NULL;
#endif
}

char * caml_dlerror(void)
{
  return (char*) dlerror();
}

#endif
#else

void * caml_dlopen(char * libname, int for_execution)
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

/* Add to [contents] the (short) names of the files contained in
   the directory named [dirname].  No entries are added for [.] and [..].
   Return 0 on success, -1 on error; set errno in the case of error. */

int caml_read_directory(char * dirname, struct ext_table * contents)
{
  DIR * d;
#ifdef HAS_DIRENT
  struct dirent * e;
#else
  struct direct * e;
#endif
  char * p;

  d = opendir(dirname);
  if (d == NULL) return -1;
  while (1) {
    e = readdir(d);
    if (e == NULL) break;
    if (strcmp(e->d_name, ".") == 0 || strcmp(e->d_name, "..") == 0) continue;
    p = caml_stat_alloc(strlen(e->d_name) + 1);
    strcpy(p, e->d_name);
    caml_ext_table_add(contents, p);
  }
  closedir(d);
  return 0;
}

/* Recover executable name from /proc/self/exe if possible */

#ifdef __linux__

int caml_executable_name(char * name, int name_len)
{
  int retcode;
  struct stat st;

  retcode = readlink("/proc/self/exe", name, name_len);
  if (retcode == -1 || retcode >= name_len) return -1;
  name[retcode] = 0;
  /* Make sure that the contents of /proc/self/exe is a regular file.
     (Old Linux kernels return an inode number instead.) */
  if (stat(name, &st) != 0) return -1;
  if (! S_ISREG(st.st_mode)) return -1;
  return 0;
}

#endif
