/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*          Xavier Leroy and Damien Doligez, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 2009 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

/* Win32 implementation of the "st" interface */

#undef _WIN32_WINNT
#define _WIN32_WINNT 0x0400
#include <windows.h>
#include <winerror.h>
#include <stdio.h>
#include <signal.h>

#include <caml/osdeps.h>

#if 1
#define TRACE(x)
#define TRACE1(x,y)
#else
#include <stdio.h>
#define TRACE(x) printf("%d: %s\n", GetCurrentThreadId(), x); fflush(stdout)
#define TRACE1(x,y) printf("%d: %s %p\n", GetCurrentThreadId(), x, (void *)y); \
                    fflush(stdout)
#endif

typedef DWORD st_retcode;

#define SIGPREEMPTION SIGTERM

/* OS-specific initialization */

static DWORD st_initialize(void)
{
  return 0;
}

/* Thread creation.  Created in detached mode if [res] is NULL. */

typedef HANDLE st_thread_id;

static DWORD st_thread_create(st_thread_id * res,
                              LPTHREAD_START_ROUTINE fn, void * arg)
{
  HANDLE h = CreateThread(NULL, 0, fn, arg, 0, NULL);
  TRACE1("st_thread_create", h);
  if (h == NULL) return GetLastError();
  if (res == NULL)
    CloseHandle(h);
  else
    *res = h;
  return 0;
}

#define ST_THREAD_FUNCTION DWORD WINAPI

/* Cleanup at thread exit */

static void st_thread_cleanup(void)
{
}

/* Thread termination */

CAMLnoreturn_start
static void st_thread_exit(void)
CAMLnoreturn_end;

static void st_thread_exit(void)
{
  TRACE("st_thread_exit");
  ExitThread(0);
}

static void st_thread_join(st_thread_id thr)
{
  TRACE1("st_thread_join", h);
  WaitForSingleObject(thr, INFINITE);
}

/* Thread-specific state */

typedef DWORD st_tlskey;

static DWORD st_tls_newkey(st_tlskey * res)
{
  *res = TlsAlloc();
  if (*res == TLS_OUT_OF_INDEXES)
    return GetLastError();
  else
    return 0;
}

Caml_inline void * st_tls_get(st_tlskey k)
{
  return TlsGetValue(k);
}

Caml_inline void st_tls_set(st_tlskey k, void * v)
{
  TlsSetValue(k, v);
}

/* The master lock.  */

typedef CRITICAL_SECTION st_masterlock;

static void st_masterlock_init(st_masterlock * m)
{
  TRACE("st_masterlock_init");
  InitializeCriticalSection(m);
  EnterCriticalSection(m);
}

Caml_inline void st_masterlock_acquire(st_masterlock * m)
{
  TRACE("st_masterlock_acquire");
  EnterCriticalSection(m);
  TRACE("st_masterlock_acquire (done)");
}

Caml_inline void st_masterlock_release(st_masterlock * m)
{
  LeaveCriticalSection(m);
  TRACE("st_masterlock_released");
}

Caml_inline int st_masterlock_waiters(st_masterlock * m)
{
  return 1;                     /* info not maintained */
}

/* Scheduling hints */

Caml_inline void st_thread_yield(st_masterlock * m)
{
  LeaveCriticalSection(m);
  Sleep(0);
  EnterCriticalSection(m);
}

/* Mutexes */

typedef CRITICAL_SECTION * st_mutex;

static DWORD st_mutex_create(st_mutex * res)
{
  st_mutex m = caml_stat_alloc_noexc(sizeof(CRITICAL_SECTION));
  if (m == NULL) return ERROR_NOT_ENOUGH_MEMORY;
  InitializeCriticalSection(m);
  *res = m;
  return 0;
}

static DWORD st_mutex_destroy(st_mutex m)
{
  DeleteCriticalSection(m);
  caml_stat_free(m);
  return 0;
}

Caml_inline DWORD st_mutex_lock(st_mutex m)
{
  TRACE1("st_mutex_lock", m);
  EnterCriticalSection(m);
  TRACE1("st_mutex_lock (done)", m);
  return 0;
}

/* Error codes with the 29th bit set are reserved for the application */

#define PREVIOUSLY_UNLOCKED 0
#define ALREADY_LOCKED (1<<29)

Caml_inline DWORD st_mutex_trylock(st_mutex m)
{
  TRACE1("st_mutex_trylock", m);
  if (TryEnterCriticalSection(m)) {
    TRACE1("st_mutex_trylock (success)", m);
    return PREVIOUSLY_UNLOCKED;
  } else {
    TRACE1("st_mutex_trylock (failure)", m);
    return ALREADY_LOCKED;
  }
}

Caml_inline DWORD st_mutex_unlock(st_mutex m)
{
  TRACE1("st_mutex_unlock", m);
  LeaveCriticalSection(m);
  return 0;
}

/* Triggered events */

typedef HANDLE st_event;

static DWORD st_event_create(st_event * res, int autoreset)
{
  st_event m =
    CreateEvent(NULL,
                autoreset ? FALSE/*autoreset*/ : TRUE/*manual reset*/,
                FALSE/*initially unset*/,
                NULL);
  TRACE1("st_event_create", m);
  if (m == NULL) return GetLastError();
  *res = m;
  return 0;
}

static DWORD st_event_destroy(st_event e)
{
  TRACE1("st_event_destroy", e);
  if (CloseHandle(e))
    return 0;
  else
    return GetLastError();
}

static DWORD st_event_trigger(st_event e)
{
  TRACE1("st_event_trigger", e);
  if (SetEvent(e))
    return 0;
  else
    return GetLastError();
}

static DWORD st_event_wait(st_event e)
{
  TRACE1("st_event_wait", e);
  if (WaitForSingleObject(e, INFINITE) == WAIT_FAILED)
    return GetLastError();
  else
    return 0;
}

/* Reporting errors */

static void st_check_error(DWORD retcode, char * msg)
{
  wchar_t err[1024];
  int errlen, msglen, ret;
  value str;

  if (retcode == 0) return;
  if (retcode == ERROR_NOT_ENOUGH_MEMORY) caml_raise_out_of_memory();
  ret = FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM|FORMAT_MESSAGE_IGNORE_INSERTS,
                      NULL,
                      retcode,
                      0,
                      err,
                      sizeof(err)/sizeof(wchar_t),
                      NULL);
  if (! ret) {
    ret =
      swprintf(err, sizeof(err)/sizeof(wchar_t), L"error code %lx", retcode);
  }
  msglen = strlen(msg);
  errlen = win_wide_char_to_multi_byte(err, ret, NULL, 0);
  str = caml_alloc_string(msglen + 2 + errlen);
  memmove (&Byte(str, 0), msg, msglen);
  memmove (&Byte(str, msglen), ": ", 2);
  win_wide_char_to_multi_byte(err, ret, &Byte(str, msglen + 2), errlen);
  caml_raise_sys_error(str);
}

/* Variable used to stop the "tick" thread */
static volatile int caml_tick_thread_stop = 0;

/* The tick thread: posts a SIGPREEMPTION signal periodically */

static DWORD WINAPI caml_thread_tick(void * arg)
{
  while(! caml_tick_thread_stop) {
    Sleep(Thread_timeout);
    /* The preemption signal should never cause a callback, so don't
     go through caml_handle_signal(), just record signal delivery via
     caml_record_signal(). */
    caml_record_signal(SIGPREEMPTION);
  }
  return 0;
}

/* "At fork" processing -- none under Win32 */

static DWORD st_atfork(void (*fn)(void))
{
  return 0;
}

/* Signal handling -- none under Win32 */

value caml_thread_sigmask(value cmd, value sigs) /* ML */
{
  caml_invalid_argument("Thread.sigmask not implemented");
  return Val_int(0);            /* not reached */
}

value caml_wait_signal(value sigs) /* ML */
{
  caml_invalid_argument("Thread.wait_signal not implemented");
  return Val_int(0);            /* not reached */
}
