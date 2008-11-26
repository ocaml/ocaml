/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*  Contributed by Sylvain Le Gall for Lexifi                          */
/*                                                                     */
/*  Copyright 2008 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../../LICENSE.  */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#include "winworker.h"
#include "winlist.h"
#include "windbug.h"
#include <mlvalues.h>
#include <alloc.h>
#include "unixsupport.h"

typedef enum {
  WORKER_CMD_NONE = 0,
  WORKER_CMD_EXEC,
  WORKER_CMD_STOP
} WORKERCMD;

struct _WORKER {
  LIST       lst;           /* This structure is used as a list. */
  HANDLE     hJobStarted;   /* Event representing that the function has begun. */
  HANDLE     hJobStop;      /* Event that can be used to notify the function that it
                               should stop processing. */
  HANDLE     hJobDone;      /* Event representing that the function has finished. */
  void      *lpJobUserData; /* User data for the job. */
  WORKERFUNC hJobFunc;      /* Function to be called during APC */
  HANDLE     hWorkerReady;  /* Worker is ready. */
  HANDLE     hCommandReady; /* Worker should execute command. */
  WORKERCMD  ECommand;      /* Command to execute */
  HANDLE     hThread;       /* Thread handle of the worker. */
};

#define THREAD_WORKERS_MAX 16
#define THREAD_WORKERS_MEM 4000

LPWORKER lpWorkers       = NULL;
DWORD    nWorkersCurrent = 0;
DWORD    nWorkersMax     = 0;
HANDLE   hWorkersMutex   = INVALID_HANDLE_VALUE;
HANDLE   hWorkerHeap     = INVALID_HANDLE_VALUE;

DWORD WINAPI worker_wait (LPVOID _data)
{
  BOOL     bExit;
  LPWORKER lpWorker;
 
  lpWorker = (LPWORKER )_data;
  bExit    = FALSE;

#ifdef DBUG
  dbug_print("Worker %x starting", lpWorker);
#endif
  while (
      !bExit 
      && SignalObjectAndWait(
        lpWorker->hWorkerReady, 
        lpWorker->hCommandReady,
        INFINITE, 
        TRUE) == WAIT_OBJECT_0)
  {
#ifdef DBUG
    dbug_print("Worker %x running", lpWorker);
#endif
    switch (lpWorker->ECommand)
    {
      case WORKER_CMD_NONE:
        break;

      case WORKER_CMD_EXEC:
        if (lpWorker->hJobFunc != NULL)
        {
          SetEvent(lpWorker->hJobStarted);
          lpWorker->hJobFunc(lpWorker->hJobStop, lpWorker->lpJobUserData);
          SetEvent(lpWorker->hJobDone);
        };
        break;

      case WORKER_CMD_STOP:
        bExit = TRUE;
        break;
    }
  };
#ifdef DBUG
  dbug_print("Worker %x exiting", lpWorker);
#endif

  return 0;
}

LPWORKER worker_new (void)
{
  LPWORKER lpWorker = NULL;

  if (!HeapLock(hWorkerHeap))
  {
    win32_maperr(GetLastError());
    uerror("worker_new", Nothing);
  };
  lpWorker = (LPWORKER)HeapAlloc(hWorkerHeap, 0, sizeof(WORKER));
  HeapUnlock(hWorkerHeap);
  list_init((LPLIST)lpWorker);
  lpWorker->hJobStarted  = CreateEvent(NULL, TRUE, FALSE, NULL);
  lpWorker->hJobStop     = CreateEvent(NULL, TRUE, FALSE, NULL);
  lpWorker->hJobDone     = CreateEvent(NULL, TRUE, FALSE, NULL);
  lpWorker->lpJobUserData = NULL;
  lpWorker->hWorkerReady       = CreateEvent(NULL, FALSE, FALSE, NULL);
  lpWorker->hCommandReady      = CreateEvent(NULL, FALSE, FALSE, NULL);
  lpWorker->ECommand           = WORKER_CMD_NONE;
  lpWorker->hThread = CreateThread(
    NULL, 
    THREAD_WORKERS_MEM, 
    worker_wait, 
    (LPVOID)lpWorker, 
    0, 
    NULL);

  return lpWorker;
};

void worker_free (LPWORKER lpWorker)
{
  /* Wait for termination of the worker */
#ifdef DBUG
  dbug_print("Shutting down worker %x", lpWorker);
#endif
  WaitForSingleObject(lpWorker->hWorkerReady, INFINITE);
  lpWorker->ECommand = WORKER_CMD_STOP;
  SetEvent(lpWorker->hCommandReady);
  WaitForSingleObject(lpWorker->hThread, INFINITE);

  /* Free resources */
#ifdef DBUG
  dbug_print("Freeing resources of worker %x", lpWorker);
#endif
  if (lpWorker->hThread != INVALID_HANDLE_VALUE)
  {
    CloseHandle(lpWorker->hThread);
    lpWorker->hThread = INVALID_HANDLE_VALUE;
  }

  if (lpWorker->hJobStarted != INVALID_HANDLE_VALUE)
  {
    CloseHandle(lpWorker->hJobStarted);
    lpWorker->hJobStarted = INVALID_HANDLE_VALUE;
  }

  if (lpWorker->hJobStop != INVALID_HANDLE_VALUE)
  {
    CloseHandle(lpWorker->hJobStop);
    lpWorker->hJobStop = INVALID_HANDLE_VALUE;
  }

  if (lpWorker->hJobDone != INVALID_HANDLE_VALUE)
  {
    CloseHandle(lpWorker->hJobDone);
    lpWorker->hJobDone = INVALID_HANDLE_VALUE;
  }

  lpWorker->lpJobUserData = NULL;
  lpWorker->hJobFunc = NULL;

  if (lpWorker->hWorkerReady != INVALID_HANDLE_VALUE)
  {
    CloseHandle(lpWorker->hWorkerReady);
    lpWorker->hWorkerReady = INVALID_HANDLE_VALUE;
  }

  if (lpWorker->hCommandReady != INVALID_HANDLE_VALUE)
  {
    CloseHandle(lpWorker->hCommandReady);
    lpWorker->hCommandReady = INVALID_HANDLE_VALUE;
  }

  if (!HeapLock(hWorkerHeap))
  {
    win32_maperr(GetLastError());
    uerror("worker_new", Nothing);
  };
  HeapFree(hWorkerHeap, 0, lpWorker);
  HeapUnlock(hWorkerHeap);
};

LPWORKER worker_pop (void)
{
  LPWORKER lpWorkerFree = NULL;

  WaitForSingleObject(hWorkersMutex, INFINITE);
  /* Get the first worker of the list */
  if (lpWorkers != NULL)
  {
    lpWorkerFree = lpWorkers;
    lpWorkers = LIST_NEXT(LPWORKER, lpWorkers);
  }
  nWorkersCurrent++;
  nWorkersMax = (nWorkersCurrent > nWorkersMax ? nWorkersCurrent : nWorkersMax);
#ifdef DBUG
  dbug_print("Workers running current/runnning max/waiting: %d/%d/%d",
      nWorkersCurrent,
      nWorkersMax,
      list_length((LPLIST)lpWorkers));
#endif
  ReleaseMutex(hWorkersMutex);

  if (lpWorkerFree == NULL)
  {
    /* We cannot find a free worker, create one. */
    lpWorkerFree = worker_new();
  }

  /* Ensure that we don't get dangling pointer to old data. */
  list_init((LPLIST)lpWorkerFree);
  lpWorkerFree->lpJobUserData = NULL;

  /* Reset events */
  ResetEvent(lpWorkerFree->hJobStarted);
  ResetEvent(lpWorkerFree->hJobStop);
  ResetEvent(lpWorkerFree->hJobDone);

  return lpWorkerFree;
}

void worker_push(LPWORKER lpWorker)
{
  BOOL bFreeWorker;

  bFreeWorker = TRUE;

  WaitForSingleObject(hWorkersMutex, INFINITE);
#ifdef DBUG
  dbug_print("Testing if we are under the maximum number of running workers");
#endif
  if (list_length((LPLIST)lpWorkers) < THREAD_WORKERS_MAX)
  {
#ifdef DBUG
    dbug_print("Saving this worker for future use");
#endif
#ifdef DBUG
    dbug_print("Next: %x", ((LPLIST)lpWorker)->lpNext);
#endif
    lpWorkers = (LPWORKER)list_concat((LPLIST)lpWorker, (LPLIST)lpWorkers);
    bFreeWorker = FALSE;
  };
  nWorkersCurrent--;
#ifdef DBUG
  dbug_print("Workers running current/runnning max/waiting: %d/%d/%d",
      nWorkersCurrent,
      nWorkersMax,
      list_length((LPLIST)lpWorkers));
#endif
  ReleaseMutex(hWorkersMutex);

  if (bFreeWorker)
  {
#ifdef DBUG
    dbug_print("Freeing worker %x", lpWorker);
#endif
    worker_free(lpWorker);
  }
}

void worker_init (void)
{
  int i = 0;

  /* Init a shared variable. The only way to ensure that no other
     worker will be at the same point is to use a critical section.
     */
#ifdef DBUG
  dbug_print("Allocating mutex for workers");
#endif
  if (hWorkersMutex == INVALID_HANDLE_VALUE)
  {
    hWorkersMutex = CreateMutex(NULL, FALSE, NULL);
  }

  if (hWorkerHeap == INVALID_HANDLE_VALUE)
  {
    hWorkerHeap = HeapCreate(0, sizeof(WORKER) * THREAD_WORKERS_MAX * 4, 0);
  }
}

void worker_cleanup(void)
{
  LPWORKER lpWorker = NULL;

  /* WARNING: we can have a race condition here, if while this code
     is executed another worker is waiting to access hWorkersMutex,
     he will never be able to get it...
     */
  if (hWorkersMutex != INVALID_HANDLE_VALUE)
  {
    WaitForSingleObject(hWorkersMutex, INFINITE);
#ifdef DBUG
    dbug_print("Freeing global resource of workers");
#endif
    /* Empty the queue of worker worker */
    while (lpWorkers != NULL)
    {
      ReleaseMutex(hWorkersMutex);
      lpWorker = worker_pop();
#ifdef DBUG
      dbug_print("Freeing worker %x", lpWorker);
#endif
      WaitForSingleObject(hWorkersMutex, INFINITE);
      worker_free(lpWorker);
    };
    ReleaseMutex(hWorkersMutex);
    
    /* Destroy associated mutex */
    CloseHandle(hWorkersMutex);
    hWorkersMutex = INVALID_HANDLE_VALUE;
  };
}

LPWORKER worker_job_submit (WORKERFUNC f, void *user_data)
{
  LPWORKER lpWorker = worker_pop();

#ifdef DBUG
  dbug_print("Waiting for worker to be ready");
#endif
  enter_blocking_section();
  WaitForSingleObject(lpWorker->hWorkerReady, INFINITE);
  ResetEvent(lpWorker->hWorkerReady);
  leave_blocking_section();
#ifdef DBUG
  dbug_print("Worker is ready");
#endif

  lpWorker->hJobFunc      = f;
  lpWorker->lpJobUserData = user_data;
  lpWorker->ECommand      = WORKER_CMD_EXEC;

#ifdef DBUG
  dbug_print("Call worker (func: %x, worker: %x)", f, lpWorker);
#endif
  SetEvent(lpWorker->hCommandReady);

  return (LPWORKER)lpWorker;
}

HANDLE worker_job_event_done (LPWORKER lpWorker)
{
  return lpWorker->hJobDone;
}

void worker_job_stop (LPWORKER lpWorker)
{
#ifdef DBUG
  dbug_print("Sending stop signal to worker %x", lpWorker);
#endif
  SetEvent(lpWorker->hJobStop);
#ifdef DBUG
  dbug_print("Signal sent to worker %x", lpWorker);
#endif
}

void worker_job_finish (LPWORKER lpWorker)
{
#ifdef DBUG
  dbug_print("Finishing call of worker %x", lpWorker);
#endif
  enter_blocking_section();
  WaitForSingleObject(lpWorker->hJobDone, INFINITE);
  leave_blocking_section();

  worker_push(lpWorker);
}
