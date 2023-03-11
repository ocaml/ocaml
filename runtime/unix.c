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
#include <sys/time.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <errno.h>
#include "caml/config.h"
#if defined(SUPPORT_DYNAMIC_LINKING) && !defined(BUILDING_LIBCAMLRUNS)
#define WITH_DYNAMIC_LINKING
#ifdef __CYGWIN__
#include "flexdll.h"
#else
#include <dlfcn.h>
#endif
#endif
#ifdef HAS_UNISTD
#include <unistd.h>
#endif
#ifdef HAS_POSIX_MONOTONIC_CLOCK
#include <time.h>
#elif HAS_MACH_ABSOLUTE_TIME
#include <mach/mach_time.h>
#endif
#ifdef HAS_DIRENT
#include <dirent.h>
#else
#include <sys/dir.h>
#endif
#ifdef __APPLE__
#include <mach-o/dyld.h>
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
  if (retcode == -1) {
    if (errno == EINTR) return Io_interrupted;
    else caml_sys_io_error(NO_ARG);
  }
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
    if (errno == EINTR) return Io_interrupted;
    if ((errno == EAGAIN || errno == EWOULDBLOCK) && n > 1) {
      /* We couldn't do a partial write here, probably because
         n <= PIPE_BUF and POSIX says that writes of less than
         PIPE_BUF characters must be atomic.
         We first try again with a partial write of 1 character.
         If that fails too, we'll return an error code. */
      n = 1; goto again;
    }
  }
  if (retcode == -1) caml_sys_io_error(NO_ARG);
  CAMLassert (retcode > 0);
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
  const char * p;
  char * dir, * fullname;
  int i;
  struct stat st;

  for (p = name; *p != 0; p++) {
    if (*p == '/') goto not_found;
  }
  for (i = 0; i < path->size; i++) {
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

#ifdef __CYGWIN__

/* Cygwin needs special treatment because of the implicit ".exe" at the
   end of executable file names */

static int cygwin_file_exists(const char * name)
{
  int fd, ret;
  struct stat st;
  /* Cannot use stat() here because it adds ".exe" implicitly */
  fd = open(name, O_RDONLY);
  if (fd == -1) return 0;
  ret = fstat(fd, &st);
  close(fd);
  return ret == 0 && S_ISREG(st.st_mode);
}

static caml_stat_string cygwin_search_exe_in_path(struct ext_table * path,
                                                  const char * name)
{
  const char * p;
  char * dir, * fullname;
  int i;

  for (p = name; *p != 0; p++) {
    if (*p == '/' || *p == '\\') goto not_found;
  }
  for (i = 0; i < path->size; i++) {
    dir = path->contents[i];
    if (dir[0] == 0) dir = ".";  /* empty path component = current dir */
    fullname = caml_stat_strconcat(3, dir, "/", name);
    if (cygwin_file_exists(fullname)) return fullname;
    caml_stat_free(fullname);
    fullname = caml_stat_strconcat(4, dir, "/", name, ".exe");
    if (cygwin_file_exists(fullname)) return fullname;
    caml_stat_free(fullname);
  }
 not_found:
  if (cygwin_file_exists(name)) return caml_stat_strdup(name);
  fullname = caml_stat_strconcat(2, name, ".exe");
  if (cygwin_file_exists(fullname)) return fullname;
  caml_stat_free(fullname);
  return caml_stat_strdup(name);
}

#endif

caml_stat_string caml_search_exe_in_path(const char * name)
{
  struct ext_table path;
  char * tofree;
  caml_stat_string res;

  caml_ext_table_init(&path, 8);
  tofree = caml_decompose_path(&path, getenv("PATH"));
#ifndef __CYGWIN__
  res = caml_search_in_path(&path, name);
#else
  res = cygwin_search_exe_in_path(&path, name);
#endif
  caml_stat_free(tofree);
  caml_ext_table_free(&path, 0);
  return res;
}

caml_stat_string caml_search_dll_in_path(struct ext_table * path,
                                         const char * name)
{
  caml_stat_string dllname;
  caml_stat_string res;

  dllname = caml_stat_strconcat(2, name, ".so");
  res = caml_search_in_path(path, dllname);
  caml_stat_free(dllname);
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
  struct dirent * e;
#else
  struct direct * e;
#endif

  d = opendir(dirname);
  if (d == NULL) return -1;
  while (1) {
    e = readdir(d);
    if (e == NULL) break;
    if (strcmp(e->d_name, ".") == 0 || strcmp(e->d_name, "..") == 0) continue;
    caml_ext_table_add(contents, caml_stat_strdup(e->d_name));
  }
  closedir(d);
  return 0;
}

/* Recover executable name from /proc/self/exe if possible */

char * caml_executable_name(void)
{
#if defined(__linux__)
  int namelen, retcode;
  char * name;
  struct stat st;

  /* lstat("/proc/self/exe") returns st_size == 0 so we cannot use it
     to determine the size of the buffer.  Instead, we guess and adjust. */
  namelen = 256;
  while (1) {
    name = caml_stat_alloc(namelen);
    retcode = readlink("/proc/self/exe", name, namelen);
    if (retcode == -1) { caml_stat_free(name); return NULL; }
    if (retcode < namelen) break;
    caml_stat_free(name);
    if (namelen >= 1024*1024) return NULL; /* avoid runaway and overflow */
    namelen *= 2;
  }
  /* readlink() does not zero-terminate its result.
     There is room for a final zero since retcode < namelen. */
  name[retcode] = 0;
  /* Make sure that the contents of /proc/self/exe is a regular file.
     (Old Linux kernels return an inode number instead.) */
  if (stat(name, &st) == -1 || ! S_ISREG(st.st_mode)) {
    caml_stat_free(name); return NULL;
  }
  return name;

#elif defined(__APPLE__)
  unsigned int namelen;
  char * name;

  namelen = 256;
  name = caml_stat_alloc(namelen);
  if (_NSGetExecutablePath(name, &namelen) == 0) return name;
  caml_stat_free(name);
  /* Buffer is too small, but namelen now contains the size needed */
  name = caml_stat_alloc(namelen);
  if (_NSGetExecutablePath(name, &namelen) == 0) return name;
  caml_stat_free(name);
  return NULL;

#else
  return NULL;

#endif
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

int64_t caml_time_counter(void)
{
#if defined(HAS_MACH_ABSOLUTE_TIME)
  static mach_timebase_info_data_t time_base = {0};
  uint64_t now;

  if (time_base.denom == 0) {
    if (mach_timebase_info(&time_base) != KERN_SUCCESS)
      return 0;
  }

  now = mach_absolute_time();
  return (int64_t)((now * time_base.numer) / time_base.denom);
#elif defined(HAS_POSIX_MONOTONIC_CLOCK)
  struct timespec t;
  clock_gettime(CLOCK_MONOTONIC, &t);
  return
    (int64_t)t.tv_sec  * (int64_t)1000000000 +
    (int64_t)t.tv_nsec;
#elif defined(HAS_GETTIMEOFDAY)
  struct timeval t;
  gettimeofday(&t, 0);
  return
    (int64_t)t.tv_sec  * (int64_t)1000000000 +
    (int64_t)t.tv_usec * (int64_t)1000;
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

/* Standard Unix implementation: reserve with mmap (and trim to alignment) with
   commit done using mmap as well. */

Caml_inline void safe_munmap(uintnat addr, uintnat size)
{
  if (size > 0) {
    caml_gc_message(0x1000, "munmap %" ARCH_INTNAT_PRINTF_FORMAT "d"
                            " bytes at %" ARCH_INTNAT_PRINTF_FORMAT "x"
                            " for heaps\n", size, addr);
    munmap((void*)addr, size);
  }
}

void *caml_plat_mem_map(uintnat size, uintnat alignment, int reserve_only)
{
  uintnat alloc_sz = size + alignment;
  uintnat base, aligned_start, aligned_end;
  void* mem;

  mem = mmap(0, alloc_sz, reserve_only ? PROT_NONE : (PROT_READ | PROT_WRITE),
             MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
  if (mem == MAP_FAILED)
    return 0;

  /* trim to an aligned region */
  base = (uintnat)mem;
  aligned_start = (base + alignment - 1) & ~(alignment - 1);
  aligned_end = aligned_start + size;
  safe_munmap(base, aligned_start - base);
  safe_munmap(aligned_end, (base + alloc_sz) - aligned_end);
  mem = (void*)aligned_start;

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

void *caml_plat_mem_map(uintnat size, uintnat alignment, int reserve_only)
{
  void* mem;

  if (alignment > caml_plat_mmap_alignment)
    caml_fatal_error("Cannot align memory to %lx on this platform", alignment);

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
