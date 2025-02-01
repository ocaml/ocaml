/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt            */
/*                                                                        */
/*   Copyright 2001 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#define CAML_INTERNALS

/* Unix-specific stuff */

#define _GNU_SOURCE
           /* Helps finding RTLD_DEFAULT in glibc */
           /* also secure_getenv */

#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <sys/ioctl.h>
#include <sys/types.h>
#include "caml/config.h"
#ifdef HAS_GETTIMEOFDAY
#include <sys/time.h>
#endif
#include <sys/stat.h>
#include <fcntl.h>
#if defined(SUPPORT_DYNAMIC_LINKING) && !defined(BUILDING_LIBCAMLRUNS)
#define WITH_DYNAMIC_LINKING
#ifdef __CYGWIN__
#include "flexdll.h"
#else
#include <dlfcn.h>
#endif
#endif
#ifndef _WIN32
#include <unistd.h>
#endif
#ifdef HAS_POSIX_MONOTONIC_CLOCK
#include <time.h>
#elif defined(HAS_CLOCK_GETTIME_NSEC_NP)
#include <time.h>
#endif
#ifdef HAS_DIRENT
#include <dirent.h>
#else
#include <sys/dir.h>
#endif
#ifdef HAS_LIBGEN_H
#include <libgen.h>
#endif
#ifdef HAS_SYS_MMAN_H
#include <sys/mman.h>
#endif
#include "caml/fail.h"
#include "caml/memory.h"
#include "caml/misc.h"
#include "caml/osdeps.h"
#include "caml/signals.h"
#include "caml/sys.h"
#include "caml/io.h"
#include "caml/alloc.h"
#include "caml/platform.h"

#ifndef S_ISREG
#define S_ISREG(mode) (((mode) & S_IFMT) == S_IFREG)
#endif

#ifndef EINTR
#define EINTR (-1)
#endif
#ifndef EAGAIN
#define EAGAIN (-1)
#endif
#ifndef EWOULDBLOCK
#define EWOULDBLOCK (-1)
#endif

int caml_read_fd(int fd, int flags, void * buf, int n)
{
  int retcode;
  caml_enter_blocking_section_no_pending();
  retcode = read(fd, buf, n);
  caml_leave_blocking_section();
  return retcode;
}

int caml_write_fd(int fd, int flags, void * buf, int n)
{
  int retcode;
 again:
  caml_enter_blocking_section_no_pending();
  retcode = write(fd, buf, n);
  caml_leave_blocking_section();
  if (retcode == -1) {
    if ((errno == EAGAIN || errno == EWOULDBLOCK) && n > 1) {
      /* We couldn't do a partial write here, probably because
         n <= PIPE_BUF and POSIX says that writes of less than
         PIPE_BUF characters must be atomic.
         We first try again with a partial write of 1 character.
         If that fails too, we'll return an error code. */
      n = 1; goto again;
    }
  }
  CAMLassert (retcode > 0 || retcode == -1);
  return retcode;
}

caml_stat_string caml_decompose_path(struct ext_table * tbl, char * path)
{
  char * p, * q;
  size_t n;

  if (path == NULL) return NULL;
  p = caml_stat_strdup(path);
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

caml_stat_string caml_search_in_path(struct ext_table * path, const char * name)
{
  const char * dir;
  char * fullname;
  struct stat st;

  for (const char *p = name; *p != 0; p++) {
    if (*p == '/') goto not_found;
  }
  for (int i = 0; i < path->size; i++) {
    dir = path->contents[i];
    if (dir[0] == 0) dir = ".";  /* empty path component = current dir */
    fullname = caml_stat_strconcat(3, dir, "/", name);
    if (stat(fullname, &st) == 0 && S_ISREG(st.st_mode))
      return fullname;
    caml_stat_free(fullname);
  }
 not_found:
  return caml_stat_strdup(name);
}

caml_stat_string caml_search_exe_in_path(const char * name)
{
  /* caml_search_in_system_path treats PATH being "Set But Null" (i.e. equal to
     "") as being equivalent to being set to "." */
  caml_stat_string res = caml_search_in_system_path(name);
  if (res == NULL)
    res = caml_stat_strdup(name);
  return res;
}

#ifdef WITH_DYNAMIC_LINKING
#ifdef __CYGWIN__
/* Use flexdll */

void * caml_dlopen(char * libname, int global)
{
  int flags = (global ? FLEXDLL_RTLD_GLOBAL : 0);
  return flexdll_dlopen(libname, flags);
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
  return flexdll_dlsym(flexdll_dlopen(NULL,0), name);
}

char * caml_dlerror(void)
{
  return flexdll_dlerror();
}

#else /* ! __CYGWIN__ */
/* Use normal dlopen */

#ifndef RTLD_GLOBAL
#define RTLD_GLOBAL 0
#endif
#ifndef RTLD_LOCAL
#define RTLD_LOCAL 0
#endif

void * caml_dlopen(char * libname, int global)
{
  return dlopen(libname, RTLD_NOW | (global ? RTLD_GLOBAL : RTLD_LOCAL));
}

void caml_dlclose(void * handle)
{
  dlclose(handle);
}

void * caml_dlsym(void * handle, const char * name)
{
  return dlsym(handle, name);
}

void * caml_globalsym(const char * name)
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

#endif /* __CYGWIN__ */
#else

void * caml_dlopen(char * libname, int global)
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

/* Add to [contents] the (short) names of the files contained in
   the directory named [dirname].  No entries are added for [.] and [..].
   Return 0 on success, -1 on error; set errno in the case of error. */

CAMLexport int caml_read_directory(char * dirname, struct ext_table * contents)
{
  DIR * d;
#ifdef HAS_DIRENT
  const struct dirent * e;
#else
  const struct direct * e;
#endif

  d = opendir(dirname);
  if (d == NULL) return -1;
  while (1) {
    e = readdir(d);
    if (e == NULL) break;
    if (strcmp(e->d_name, ".") == 0 || strcmp(e->d_name, "..") == 0) continue;
    int rc = caml_ext_table_add_noexc(contents, caml_stat_strdup(e->d_name));
    if (rc == -1) { closedir(d); errno = ENOMEM; return -1; }
  }
  closedir(d);
  return 0;
}

char *caml_secure_getenv (char const *var)
{
#ifdef HAS_SECURE_GETENV
  return secure_getenv (var);
#elif defined (HAS___SECURE_GETENV)
  return __secure_getenv (var);
#elif defined(HAS_ISSETUGID)
  if (!issetugid ())
    return getenv(var);
  else
    return NULL;
#else
  if (geteuid () == getuid () && getegid () == getgid ())
    return getenv(var);
  else
    return NULL;
#endif
}

uint64_t caml_time_counter(void)
{
#if defined(HAS_CLOCK_GETTIME_NSEC_NP)
  return (clock_gettime_nsec_np(CLOCK_UPTIME_RAW));
#elif defined(HAS_POSIX_MONOTONIC_CLOCK)
  struct timespec t;
  clock_gettime(CLOCK_MONOTONIC, &t);
  return
    (uint64_t) t.tv_sec  * NSEC_PER_SEC +
    (uint64_t) t.tv_nsec;
#elif defined(HAS_GETTIMEOFDAY)
  struct timeval t;
  gettimeofday(&t, 0);
  return
    (uint64_t) t.tv_sec  * NSEC_PER_SEC +
    (uint64_t) t.tv_usec * NSEC_PER_USEC;
#else
# error "No timesource available"
#endif
}



int caml_num_rows_fd(int fd)
{
#ifdef TIOCGWINSZ
  struct winsize w;
  w.ws_row = -1;
  if (ioctl(fd, TIOCGWINSZ, &w) == 0)
    return w.ws_row;
  else
    return -1;
#else
  return -1;
#endif
}

void caml_init_os_params(void)
{
  caml_plat_mmap_alignment = caml_plat_pagesize = sysconf(_SC_PAGESIZE);
  return;
}

#ifndef __CYGWIN__

void *caml_plat_mem_map(uintnat size, int reserve_only)
{
  uintnat alloc_sz = size;
  void* mem;

  mem = mmap(0, alloc_sz, reserve_only ? PROT_NONE : (PROT_READ | PROT_WRITE),
             MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
  if (mem == MAP_FAILED)
    return 0;

  return mem;
}

static void* map_fixed(void* mem, uintnat size, int prot)
{
  if (mmap(mem, size, prot, MAP_PRIVATE | MAP_ANONYMOUS | MAP_FIXED,
           -1, 0) == MAP_FAILED) {
    return 0;
  } else {
    return mem;
  }
}

#else

/* Cygwin implementation: memory reserved using mmap, but relying on the large
   allocation granularity of the underlying Windows VirtualAlloc call to ensure
   alignment (since on Windows it is not possible to trim the region). Commit
   done using mprotect, since Cygwin's mmap doesn't implement the required
   functions for committing using mmap. */

void *caml_plat_mem_map(uintnat size, int reserve_only)
{
  void* mem;

  mem = mmap(0, size, reserve_only ? PROT_NONE : (PROT_READ | PROT_WRITE),
             MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
  if (mem == MAP_FAILED)
    return 0;

  return mem;
}

static void* map_fixed(void* mem, uintnat size, int prot)
{
  if (mprotect(mem, size, prot) != 0) {
    return 0;
  } else {
    return mem;
  }
}

#endif /* !__CYGWIN__ */

void* caml_plat_mem_commit(void* mem, uintnat size)
{
  void* p = map_fixed(mem, size, PROT_READ | PROT_WRITE);
  /*
    FIXME: On Linux, it might be useful to populate page tables with
    MAP_POPULATE to reduce the time spent blocking on page faults at
    a later point.
  */
  return p;
}

void caml_plat_mem_decommit(void* mem, uintnat size)
{
  map_fixed(mem, size, PROT_NONE);
}

void caml_plat_mem_unmap(void* mem, uintnat size)
{
  if (munmap(mem, size) != 0)
    CAMLassert(0);
}

static char * caml_dirname (const char * path)
{
#ifdef HAS_LIBGEN_H
  char *dir, *res;
  dir = caml_stat_strdup(path);
  res = caml_stat_strdup(dirname(dir));
  caml_stat_free(dir);
  return res;
#else
  /* See Filename.generic_dirname */
  ptrdiff_t n = strlen(path) - 1;
  char *res;
  if (n < 0) /* path is "" */
    return caml_stat_strdup(".");
  while (n >= 0 && path[n] == '/')
    n--;
  if (n < 0) /* path is entirely slashes */
    return caml_stat_strdup("/");
  while (n >= 0 && path[n] != '/')
    n--;
  if (n < 0) /* path is relative */
    return caml_stat_strdup(".");
  while (n >= 0 && path[n] == '/')
    n--;
  if (n < 0) /* path is a file at root */
    return caml_stat_strdup("/");
  /* n is the _index_ of the last character of the dirname */
  res = caml_stat_alloc(n + 2);
  memcpy(res, path, n + 1);
  res[n + 1] = 0;
  return res;
#endif
}

CAMLextern char_os* caml_locate_standard_library (const char *exe_name,
                                                  const char *stdlib_default,
                                                  char **dirname)
{
  if (Is_relative_dir(stdlib_default)) {
    char * root = caml_dirname(exe_name);
    char * candidate =
      caml_stat_strconcat(3, root, CAML_DIR_SEP, stdlib_default);
    /* In practice, a system which can be configured --with-relative-libdir will
       also have realpath. The directory is normalised here for consistency with
       the behaviour on Windows, which doesn't have a direct equivalent of
       dirname and performs the equivalent of realpath as a side-effect of
       determining the root path. */
#ifdef HAS_REALPATH
    char * resolved_candidate = realpath(candidate, NULL);
    /* If realpath fails, use the non-normalised path for error messages. */
    if (resolved_candidate != NULL) {
      caml_stat_free(candidate);
      /* realpath uses malloc */
      candidate = caml_stat_strdup(resolved_candidate);
      free(resolved_candidate);
    }
#endif
    if (dirname == NULL)
      caml_stat_free(root);
    else
      *dirname = root;
    return candidate;
  } else {
    return caml_stat_strdup(stdlib_default);
  }
}
