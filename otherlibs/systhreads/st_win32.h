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

/* Unique thread identifiers and atomic operations over them */
#ifdef ARCH_SIXTYFOUR
typedef LONG64 st_tid;
#define Tid_Atomic_Exchange InterlockedExchange64
#define Tid_Atomic_Compare_Exchange InterlockedCompareExchange64
#else
typedef LONG st_tid;
#define Tid_Atomic_Exchange InterlockedExchange
#define Tid_Atomic_Compare_Exchange InterlockedCompareExchange
#endif

/* Thread-local storage associating a Win32 event to every thread. */
static DWORD st_thread_sem_key;

/* Thread-local storage for the OCaml thread ID. */
static DWORD st_thread_id_key;

/* OS-specific initialization */

static DWORD st_initialize(void)
{
  DWORD result = 0;
  st_thread_sem_key = TlsAlloc();
  if (st_thread_sem_key == TLS_OUT_OF_INDEXES)
    return GetLastError();
  st_thread_id_key = TlsAlloc();
  if (st_thread_id_key == TLS_OUT_OF_INDEXES) {
    result = GetLastError();
    TlsFree(st_thread_sem_key);
  }
  return result;
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
  HANDLE ev = (HANDLE) TlsGetValue(st_thread_sem_key);
  if (ev != NULL) CloseHandle(ev);
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

/* OS-specific handling of the OCaml thread ID (must be called with the runtime
   lock). */
Caml_inline void st_thread_set_id(intnat id)
{
  CAMLassert(id != 0);
  st_tls_set(st_thread_id_key, (void *)id);
}

/* Return the identifier for the current thread. The 0 value is reserved. */
Caml_inline intnat st_current_thread_id(void)
{
  intnat id = (intnat)st_tls_get(st_thread_id_key);
  CAMLassert(id != 0);
  return id;
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

struct st_mutex_ {
  CRITICAL_SECTION crit;
  volatile st_tid owner;    /* 0 if unlocked */
  /* The "owner" field is not always protected by "crit"; it is also
     accessed without holding "crit", using the Interlocked API for
     atomic accesses */
};

typedef struct st_mutex_ * st_mutex;

static DWORD st_mutex_create(st_mutex * res)
{
  st_mutex m = caml_stat_alloc_noexc(sizeof(struct st_mutex_));
  if (m == NULL) return ERROR_NOT_ENOUGH_MEMORY;
  InitializeCriticalSection(&m->crit);
  m->owner = 0;
  *res = m;
  return 0;
}

static DWORD st_mutex_destroy(st_mutex m)
{
  DeleteCriticalSection(&m->crit);
  caml_stat_free(m);
  return 0;
}

/* Error codes with the 29th bit set are reserved for the application */

#define MUTEX_DEADLOCK (1<<29 | 1)
#define MUTEX_PREVIOUSLY_UNLOCKED 0
#define MUTEX_ALREADY_LOCKED (1 << 29)
#define MUTEX_NOT_OWNED (1<<29 | 2)

Caml_inline DWORD st_mutex_lock(st_mutex m)
{
  st_tid self, prev;
  TRACE1("st_mutex_lock", m);
  self = st_current_thread_id();
  /* Critical sections are recursive locks, so this will succeed
     if we already own the lock */
  EnterCriticalSection(&m->crit);
  /* Record that we are the owner of the lock */
  prev = Tid_Atomic_Exchange(&m->owner, self);
  if (prev != 0) {
    /* The mutex was already locked by ourselves.
       Cancel the EnterCriticalSection above and return an error. */
    TRACE1("st_mutex_lock (deadlock)", m);
    LeaveCriticalSection(&m->crit);
    return MUTEX_DEADLOCK;
  }
  TRACE1("st_mutex_lock (done)", m);
  return 0;
}

Caml_inline DWORD st_mutex_trylock(st_mutex m)
{
  st_tid self, prev;
  TRACE1("st_mutex_trylock", m);
  self = st_current_thread_id();
  if (! TryEnterCriticalSection(&m->crit)) {
    TRACE1("st_mutex_trylock (failure)", m);
    return MUTEX_ALREADY_LOCKED;
  }
  /* Record that we are the owner of the lock */
  prev = Tid_Atomic_Exchange(&m->owner, self);
  if (prev != 0) {
    /* The mutex was already locked by ourselves.
       Cancel the EnterCriticalSection above and return "already locked". */
    TRACE1("st_mutex_trylock (already locked by self)", m);
    LeaveCriticalSection(&m->crit);
    return MUTEX_ALREADY_LOCKED;
  }
  TRACE1("st_mutex_trylock (done)", m);
  return MUTEX_PREVIOUSLY_UNLOCKED;
}

Caml_inline DWORD st_mutex_unlock(st_mutex m)
{
  st_tid self, prev;
  /* If the calling thread holds the lock, m->owner is stable and equal
     to st_current_thread_id().
     Otherwise, the value of m->owner can be 0 (if the mutex is unlocked)
     or some other thread ID (if the mutex is held by another thread),
     but is never equal to st_current_thread_id(). */
  self = st_current_thread_id();
  prev = Tid_Atomic_Compare_Exchange(&m->owner, 0, self);
  if (prev != self) {
    /* The value of m->owner is unchanged */
    TRACE1("st_mutex_unlock (error)", m);
    return MUTEX_NOT_OWNED;
  }
  TRACE1("st_mutex_unlock", m);
  LeaveCriticalSection(&m->crit);
  return 0;
}

/* Condition variables */

/* A condition variable is just a list of threads currently
   waiting on this c.v.  Each thread is represented by its
   associated event. */

struct st_wait_list {
  HANDLE event;                  /* event of the first waiting thread */
  struct st_wait_list * next;
};

typedef struct st_condvar_struct {
  CRITICAL_SECTION lock;         /* protect the data structure */
  struct st_wait_list * waiters; /* list of threads waiting */
} * st_condvar;

static DWORD st_condvar_create(st_condvar * res)
{
  st_condvar c = caml_stat_alloc_noexc(sizeof(struct st_condvar_struct));
  if (c == NULL) return ERROR_NOT_ENOUGH_MEMORY;
  InitializeCriticalSection(&c->lock);
  c->waiters = NULL;
  *res = c;
  return 0;
}

static DWORD st_condvar_destroy(st_condvar c)
{
  TRACE1("st_condvar_destroy", c);
  DeleteCriticalSection(&c->lock);
  caml_stat_free(c);
  return 0;
}

static DWORD st_condvar_signal(st_condvar c)
{
  DWORD rc = 0;
  struct st_wait_list * curr, * next;

  TRACE1("st_condvar_signal", c);
  EnterCriticalSection(&c->lock);
  curr = c->waiters;
  if (curr != NULL) {
    next = curr->next;
    /* Wake up the first waiting thread */
    TRACE1("st_condvar_signal: waking up", curr->event);
    if (! SetEvent(curr->event)) rc = GetLastError();
    /* Remove it from the waiting list */
    c->waiters = next;
  }
  LeaveCriticalSection(&c->lock);
  return rc;
}

static DWORD st_condvar_broadcast(st_condvar c)
{
  DWORD rc = 0;
  struct st_wait_list * curr, * next;

  TRACE1("st_condvar_broadcast", c);
  EnterCriticalSection(&c->lock);
  /* Wake up all waiting threads */
  curr = c->waiters;
  while (curr != NULL) {
    next = curr->next;
    TRACE1("st_condvar_signal: waking up", curr->event);
    if (! SetEvent(curr->event)) rc = GetLastError();
    curr = next;
  }
  /* Remove them all from the waiting list */
  c->waiters = NULL;
  LeaveCriticalSection(&c->lock);
  return rc;
}

static DWORD st_condvar_wait(st_condvar c, st_mutex m)
{
  HANDLE ev;
  struct st_wait_list wait;
  DWORD rc;
  st_tid self, prev;

  TRACE1("st_condvar_wait", c);
  /* Recover (or create) the event associated with the calling thread */
  ev = (HANDLE) TlsGetValue(st_thread_sem_key);
  if (ev == 0) {
    ev = CreateEvent(NULL,
                     FALSE /*auto reset*/,
                     FALSE /*initially unset*/,
                     NULL);
    if (ev == NULL) return GetLastError();
    TlsSetValue(st_thread_sem_key, (void *) ev);
  }
  /* Get ready to release the mutex */
  self = st_current_thread_id();
  prev = Tid_Atomic_Compare_Exchange(&m->owner, 0, self);
  if (prev != self) {
    /* The value of m->owner is unchanged */
    TRACE1("st_condvar_wait: error: mutex not held", m);
    return MUTEX_NOT_OWNED;
  }
  /* Insert the current thread in the waiting list (atomically) */
  EnterCriticalSection(&c->lock);
  wait.event = ev;
  wait.next = c->waiters;
  c->waiters = &wait;
  LeaveCriticalSection(&c->lock);
  /* Finish releasing the mutex m (like st_mutex_unlock does, minus
     the error checking, which we've already done above). */
  LeaveCriticalSection(&m->crit);
  /* Wait for our event to be signaled.  There is no risk of lost
     wakeup, since we inserted ourselves on the waiting list of c
     before releasing m */
  TRACE1("st_condvar_wait: blocking on event", ev);
  if (WaitForSingleObject(ev, INFINITE) == WAIT_FAILED)
    return GetLastError();
  /* Reacquire the mutex m */
  TRACE1("st_condvar_wait: restarted, acquiring mutex", c);
  rc = st_mutex_lock(m);
  if (rc != 0) return rc;
  TRACE1("st_condvar_wait: acquired mutex", c);
  return 0;
}

/* Triggered events */

typedef HANDLE st_event;

static DWORD st_event_create(st_event * res)
{
  st_event m =
    CreateEvent(NULL, TRUE/*manual reset*/, FALSE/*initially unset*/, NULL);
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
  switch (retcode) {
  case MUTEX_DEADLOCK:
    ret = swprintf(err, sizeof(err)/sizeof(wchar_t),
                   L"Mutex is already locked by calling thread");
    break;
  case MUTEX_NOT_OWNED:
    ret = swprintf(err, sizeof(err)/sizeof(wchar_t),
                   L"Mutex is not locked by calling thread");
    break;
  default:
    ret = FormatMessage(
             FORMAT_MESSAGE_FROM_SYSTEM|FORMAT_MESSAGE_IGNORE_INSERTS,
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
