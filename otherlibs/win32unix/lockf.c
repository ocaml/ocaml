/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*  Contributed by Tracy Camp, PolyServe Inc., <campt@polyserve.com>   */
/*                                                                     */
/*  Copyright 2002 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../../LICENSE.  */
/*  under the terms of the GNU Library General Public License.         */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#include <errno.h>
#include <fcntl.h>
#include <mlvalues.h>
#include <fail.h>
#include "unixsupport.h"
#include <stdio.h>

/*

Commands for Unix.lockf:

type lock_command =

  | F_ULOCK (* Unlock a region *) 

  | F_LOCK (* Lock a region for writing, and block if already locked *)

  | F_TLOCK (* Lock a region for writing, or fail if already locked *)

  | F_TEST (* Test a region for other process locks *)

  | F_RLOCK (* Lock a region for reading, and block if already locked *)

  | F_TRLOCK  (* Lock a region for reading, or fail if already locked *)


val lockf : file_descr -> lock_command -> int -> unitlockf fd cmd size 

puts a lock on a region of the file opened as fd. The region starts at the current
 read/write position for fd (as set by Unix.lseek), and extends size bytes
 forward if size is positive, size bytes backwards if size is negative, or 
 to the end of the file if size is zero. A write lock (set with F_LOCK or
 F_TLOCK) prevents any other process from acquiring a read or write lock on
 the region. A read lock (set with F_RLOCK or F_TRLOCK) prevents any other
 process from acquiring a write lock on the region, but lets other processes
 acquire read locks on it.
*/

#ifndef INVALID_SET_FILE_POINTER
#define INVALID_SET_FILE_POINTER (-1)
#endif

static void set_file_pointer(HANDLE h, LARGE_INTEGER dest,
                             PLARGE_INTEGER cur, DWORD method)
{
  LONG high = dest.HighPart;
  DWORD ret = SetFilePointer(h, dest.LowPart, &high, method);
  if (ret == INVALID_SET_FILE_POINTER) {
    DWORD err = GetLastError();
    if (err != NO_ERROR) { win32_maperr(err); uerror("lockf", Nothing); }
  }
  if (cur != NULL) { cur->LowPart = ret; cur->HighPart = high; }
}

CAMLprim value unix_lockf(value fd, value cmd, value span)
{
        int ret;
        OVERLAPPED overlap;
        DWORD l_start;
        DWORD l_len;
        HANDLE h;
        OSVERSIONINFO VersionInfo;
        LARGE_INTEGER cur_position;
        LARGE_INTEGER end_position;
        LARGE_INTEGER offset_position;

        VersionInfo.dwOSVersionInfoSize = sizeof(OSVERSIONINFO);
        if(GetVersionEx(&VersionInfo) == 0)
                {
                invalid_argument("lockf only supported on WIN32_NT platforms: could not determine current platform.");
                }
/* file locking only exists on NT versions */
        if(VersionInfo.dwPlatformId != VER_PLATFORM_WIN32_NT)
                {
                invalid_argument("lockf only supported on WIN32_NT platforms");
                }

<<<<<<< .courant
        h = Handle_val(fd);
=======
  h = Handle_val(fd);

  l_len = Long_val(span);
>>>>>>> .fusion-droit.r10497

        overlap.Offset = 0;
        overlap.OffsetHigh = 0;
        overlap.hEvent = 0;
        l_len = Long_val(span);

        offset_position.HighPart = 0;
        cur_position.HighPart = 0;
        end_position.HighPart = 0;
        offset_position.LowPart = 0;
        cur_position.LowPart = 0;
        end_position.LowPart = 0;

        if(l_len == 0)
                {
/* save current pointer */
                set_file_pointer(h,offset_position,&cur_position,FILE_CURRENT);
/* set to end and query */
                set_file_pointer(h,offset_position,&end_position,FILE_END);
                l_len = end_position.LowPart;
/* restore previous current pointer */
                set_file_pointer(h,cur_position,NULL,FILE_BEGIN);
                }
        else 
                {
                if (l_len < 0) 
                        {
                        set_file_pointer(h,offset_position,&cur_position,FILE_CURRENT);
                        l_len = abs(l_len);
                        if(l_len > cur_position.LowPart)
                                {
                                errno = EINVAL;
                                uerror("lockf", Nothing);
                                return Val_unit;
                                }
                        overlap.Offset = cur_position.LowPart - l_len;
                        } 
                }
  switch (Int_val(cmd)) 
        {
        case 0: /* F_ULOCK */
                if(UnlockFileEx(h, 0, l_len,0,&overlap) == 0)
                        {
                        errno = EACCES;
                        ret = -1;
                        }
                break;
        case 1: /* F_LOCK */
/* this should block until write lock is obtained */
                if(LockFileEx(h,LOCKFILE_EXCLUSIVE_LOCK,0,l_len,0,&overlap) == 0)
                        {
                        errno = EACCES;
                        ret = -1;
                        }
                break;
        case 2: /* F_TLOCK */
/* 
 * this should return immediately if write lock can-not
 * be obtained.
 */
                if(LockFileEx(h,LOCKFILE_FAIL_IMMEDIATELY | LOCKFILE_EXCLUSIVE_LOCK,0,l_len,0,&overlap) == 0)
                        {
                        errno = EACCES;
                        ret = -1;
                        }
                break;
        case 3: /* F_TEST */
/*  
 * I'm doing this by aquiring an immediate write
 * lock and then releasing it. It is not clear that
 * this behavior matches anything in particular, but
 * it is not clear the nature of the lock test performed
 * by ocaml (unix) currently.
 */
                if(LockFileEx(h,LOCKFILE_FAIL_IMMEDIATELY | LOCKFILE_EXCLUSIVE_LOCK,0,l_len,0,&overlap) == 0)
                        {
                        errno = EACCES;
                        ret = -1;
                        }
                else
                        {
                        UnlockFileEx(h, 0, l_len,0,&overlap);
                        ret = 0;
                        }
                break;
        case 4: /* F_RLOCK */
/* this should block until read lock is obtained */
                if(LockFileEx(h,0,0,l_len,0,&overlap) == 0)
                        {
                        errno = EACCES;
                        ret = -1;
                        }
                break;
        case 5: /* F_TRLOCK */
/* 
 * this should return immediately if read lock can-not
 * be obtained.
 */
                if(LockFileEx(h,LOCKFILE_FAIL_IMMEDIATELY,0,l_len,0,&overlap) == 0)
                        {
                        errno = EACCES;
                        ret = -1;
                        }
                break;
        default:
                errno = EINVAL;
                ret = -1;
        }
  if (ret == -1) uerror("lockf", Nothing);
  return Val_unit;
}

<<<<<<< .courant
=======
  switch(Int_val(cmd)) {
  case 0: /* F_ULOCK - unlock */
    if (! UnlockFileEx(h, 0,
                       lock_len.LowPart, lock_len.HighPart, &overlap))
      err = GetLastError();
    break;
  case 1: /* F_LOCK - blocking write lock */
    enter_blocking_section();
    if (! LockFileEx(h, LOCKFILE_EXCLUSIVE_LOCK, 0,
                     lock_len.LowPart, lock_len.HighPart, &overlap))
      err = GetLastError();
    leave_blocking_section();
    break;
  case 2: /* F_TLOCK - non-blocking write lock */
    if (! LockFileEx(h, LOCKFILE_FAIL_IMMEDIATELY | LOCKFILE_EXCLUSIVE_LOCK, 0,
                     lock_len.LowPart, lock_len.HighPart, &overlap))
      err = GetLastError();
    break;
  case 3: /* F_TEST - check whether a write lock can be obtained */
    /*  I'm doing this by aquiring an immediate write
     * lock and then releasing it. It is not clear that
     * this behavior matches anything in particular, but
     * it is not clear the nature of the lock test performed
     * by ocaml (unix) currently. */
    if (LockFileEx(h, LOCKFILE_FAIL_IMMEDIATELY | LOCKFILE_EXCLUSIVE_LOCK, 0,
                   lock_len.LowPart, lock_len.HighPart, &overlap)) {
      UnlockFileEx(h, 0, lock_len.LowPart, lock_len.HighPart, &overlap);
    } else {
      err = GetLastError();
    }
    break;
  case 4: /* F_RLOCK - blocking read lock */
    enter_blocking_section();
    if (! LockFileEx(h, 0, 0,
                     lock_len.LowPart, lock_len.HighPart, &overlap))
      err = GetLastError();
    leave_blocking_section();
    break;
  case 5: /* F_TRLOCK - non-blocking read lock */
    if (! LockFileEx(h, LOCKFILE_FAIL_IMMEDIATELY, 0,
                     lock_len.LowPart, lock_len.HighPart, &overlap))
      err = GetLastError();
    break;
  default:
    errno = EINVAL;
    uerror("lockf", Nothing);
  }
  if (err != NO_ERROR) {
    win32_maperr(err);
    uerror("lockf", Nothing);
  }
  CAMLreturn(Val_unit);
}
>>>>>>> .fusion-droit.r10497
