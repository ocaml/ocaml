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

#include <mlvalues.h>
#include <alloc.h>
#include <memory.h>
#include <signals.h>
#include <winsock2.h>
#include <windows.h>
#include "unixsupport.h"
#include "windbug.h"
#include "winworker.h"
#include "winlist.h"

/* This constant define the maximum number of objects that
 * can be handle by a SELECTDATA.
 * It takes the following parameters into account:
 * - limitation on number of objects is mostly due to limitation
 *   a WaitForMultipleObjects
 * - there is always an event "hStop" to watch 
 *
 * This lead to pick the following value as the biggest possible
 * value
 */
#define MAXIMUM_SELECT_OBJECTS (MAXIMUM_WAIT_OBJECTS - 1)

/* Manage set of handle */
typedef struct _SELECTHANDLESET {
  LPHANDLE lpHdl;
  DWORD    nMax;
  DWORD    nLast;
} SELECTHANDLESET;

typedef SELECTHANDLESET *LPSELECTHANDLESET;

void handle_set_init (LPSELECTHANDLESET hds, LPHANDLE lpHdl, DWORD max)
{
  DWORD i;

  hds->lpHdl = lpHdl;
  hds->nMax  = max;
  hds->nLast = 0;

  /* Set to invalid value every entry of the handle */
  for (i = 0; i < hds->nMax; i++)
  {
    hds->lpHdl[i] = INVALID_HANDLE_VALUE;
  };
}

void handle_set_add (LPSELECTHANDLESET hds, HANDLE hdl)
{
  LPSELECTHANDLESET res;

  if (hds->nLast < hds->nMax)
  {
    hds->lpHdl[hds->nLast] = hdl;
    hds->nLast++;
  }

#ifdef DBUG
  dbug_print("Adding handle %x to set %x", hdl, hds);
#endif
}

BOOL handle_set_mem (LPSELECTHANDLESET hds, HANDLE hdl)
{
  BOOL  res;
  DWORD i;

  res = FALSE;
  for (i = 0; !res && i < hds->nLast; i++)
  {
    res = (hds->lpHdl[i] == hdl);
  }

  return res;
}

void handle_set_reset (LPSELECTHANDLESET hds)
{
  DWORD i;

  for (i = 0; i < hds->nMax; i++)
  {
    hds->lpHdl[i] = INVALID_HANDLE_VALUE;
  }
  hds->nMax  = 0;
  hds->nLast = 0;
  hds->lpHdl = NULL;
}

/* Data structure for handling select */

typedef enum _SELECTHANDLETYPE {
  SELECT_HANDLE_NONE = 0,
  SELECT_HANDLE_DISK,
  SELECT_HANDLE_CONSOLE,
  SELECT_HANDLE_PIPE,
  SELECT_HANDLE_SOCKET,
} SELECTHANDLETYPE;

typedef enum _SELECTMODE {
  SELECT_MODE_NONE = 0,
  SELECT_MODE_READ,
  SELECT_MODE_WRITE, 
  SELECT_MODE_EXCEPT,
} SELECTMODE;

typedef enum _SELECTSTATE {
  SELECT_STATE_NONE = 0,
  SELECT_STATE_INITFAILED,
  SELECT_STATE_ERROR,
  SELECT_STATE_SIGNALED
} SELECTSTATE;

typedef enum _SELECTTYPE {
  SELECT_TYPE_NONE = 0,
  SELECT_TYPE_STATIC,       /* Result is known without running anything */
  SELECT_TYPE_CONSOLE_READ, /* Reading data on console */
  SELECT_TYPE_PIPE_READ,    /* Reading data on pipe */
  SELECT_TYPE_SOCKET        /* Classic select */
} SELECTTYPE;

/* Data structure for results */
typedef struct _SELECTRESULT {
  LIST       lst;
  SELECTMODE EMode;
  LPVOID     lpOrig;
} SELECTRESULT;

typedef SELECTRESULT *LPSELECTRESULT;

/* Data structure for query */
typedef struct _SELECTQUERY {
  LIST       lst;
  SELECTMODE EMode;
  HANDLE     hFileDescr;
  LPVOID     lpOrig;
} SELECTQUERY;

typedef SELECTQUERY *LPSELECTQUERY;

typedef struct _SELECTDATA {
  LIST             lst;
  SELECTTYPE       EType;
  SELECTRESULT     aResults[MAXIMUM_SELECT_OBJECTS];
  DWORD            nResultsCount;
  /* Data following are dedicated to APC like call, they
     will be initialized if required.
     */
  WORKERFUNC       funcWorker;
  SELECTQUERY      aQueries[MAXIMUM_SELECT_OBJECTS];
  DWORD            nQueriesCount;
  SELECTSTATE      EState;
  DWORD            nError;
  LPWORKER         lpWorker;
} SELECTDATA;

typedef SELECTDATA *LPSELECTDATA;

/* Get error status if associated condition is false */
static BOOL check_error(LPSELECTDATA lpSelectData, BOOL bFailed)
{
  if (bFailed && lpSelectData->nError == 0)
  {
    lpSelectData->EState = SELECT_STATE_ERROR;
    lpSelectData->nError = GetLastError();
  }
  return bFailed;
}

/* Create data associated with a  select operation */
LPSELECTDATA select_data_new (LPSELECTDATA lpSelectData, SELECTTYPE EType)
{
  /* Allocate the data structure */
  LPSELECTDATA res;
  DWORD        i;
  
  if (!HeapLock(GetProcessHeap()))
  {
    win32_maperr(GetLastError());
    uerror("select", Nothing);
  }
  res = (LPSELECTDATA)HeapAlloc(GetProcessHeap(), 0, sizeof(SELECTDATA)); 
  HeapUnlock(GetProcessHeap());

  /* Init common data */
  list_init((LPLIST)res);
  list_next_set((LPLIST)res, (LPLIST)lpSelectData);
  res->EType         = EType;
  res->nResultsCount = 0;
        

  /* Data following are dedicated to APC like call, they
     will be initialized if required. For now they are set to 
     invalid values.
     */
  res->funcWorker    = NULL;
  res->nQueriesCount = 0;
  res->EState        = SELECT_STATE_NONE;
  res->nError        = 0;
  res->lpWorker  = NULL;

  return res;
}

/* Free select data */
void select_data_free (LPSELECTDATA lpSelectData)
{
  DWORD i;

#ifdef DBUG
  dbug_print("Freeing data of %x", lpSelectData);
#endif

  /* Free APC related data, if they exists */
  if (lpSelectData->lpWorker != NULL)
  {
    worker_job_finish(lpSelectData->lpWorker);
    lpSelectData->lpWorker = NULL;
  };

  /* Make sure results/queries cannot be accessed */
  lpSelectData->nResultsCount = 0;
  lpSelectData->nQueriesCount = 0;

  if (!HeapLock(GetProcessHeap()))
  {
    win32_maperr(GetLastError());
    uerror("select_data_free", Nothing);
  };
  HeapFree(GetProcessHeap(), 0, lpSelectData);
  HeapUnlock(GetProcessHeap());
}

/* Add a result to select data, return zero if something goes wrong. */
DWORD select_data_result_add (LPSELECTDATA lpSelectData, SELECTMODE EMode, LPVOID lpOrig)
{
  DWORD res;
  DWORD i;

  res = 0;
  if (lpSelectData->nResultsCount < MAXIMUM_SELECT_OBJECTS)
  {
    i = lpSelectData->nResultsCount;
    lpSelectData->aResults[i].EMode  = EMode;
    lpSelectData->aResults[i].lpOrig = lpOrig;
    lpSelectData->nResultsCount++;
    res = 1;
  }

  return res;
}

/* Add a query to select data, return zero if something goes wrong */
DWORD select_data_query_add (LPSELECTDATA lpSelectData, SELECTMODE EMode, HANDLE hFileDescr, LPVOID lpOrig)
{
  DWORD res;
  DWORD i; 

  res = 0;
  if (lpSelectData->nQueriesCount < MAXIMUM_SELECT_OBJECTS)
  {
    i = lpSelectData->nQueriesCount;
    lpSelectData->aQueries[i].EMode      = EMode;
    lpSelectData->aQueries[i].hFileDescr = hFileDescr;
    lpSelectData->aQueries[i].lpOrig     = lpOrig;
    lpSelectData->nQueriesCount++;
    res = 1;
  }

  return res;
}

/* Search for a job that has available query slots and that match provided type.
 * If none is found, create a new one. Return the corresponding SELECTDATA, and 
 * update provided SELECTDATA head, if required.
 */
LPSELECTDATA select_data_job_search (LPSELECTDATA *lppSelectData, SELECTTYPE EType)
{
  LPSELECTDATA res;
  
  res = NULL;
  
  /* Search for job */
#ifdef DBUG
  dbug_print("Searching an available job for type %d", EType);
#endif
  res = *lppSelectData;
  while (
      res != NULL
      && !(
        res->EType == EType 
        && res->nQueriesCount < MAXIMUM_SELECT_OBJECTS
        )
      )
  {
    res = LIST_NEXT(LPSELECTDATA, res);
  }

  /* No matching job found, create one */
  if (res == NULL)
  {
#ifdef DBUG
    dbug_print("No job for type %d found, create one", EType);
#endif
    res = select_data_new(*lppSelectData, EType);
    *lppSelectData = res;
  }

  return res;
}

/***********************/
/*      Console        */
/***********************/

void read_console_poll(HANDLE hStop, void *_data)
{
  HANDLE events[2];
  INPUT_RECORD record;
  DWORD waitRes;
  DWORD n;
  LPSELECTDATA  lpSelectData;
  LPSELECTQUERY lpQuery;
  
#ifdef DBUG
  dbug_print("Waiting for data on console");
#endif

  record;
  waitRes = 0;
  n = 0;
  lpSelectData = (LPSELECTDATA)_data;
  lpQuery = &(lpSelectData->aQueries[0]);

  events[0] = hStop;
  events[1] = lpQuery->hFileDescr;
  while (lpSelectData->EState == SELECT_STATE_NONE)
  {    
    waitRes = WaitForMultipleObjects(2, events, FALSE, INFINITE);
    if (waitRes == WAIT_OBJECT_0 || check_error(lpSelectData, waitRes == WAIT_FAILED))
    {
      /* stop worker event or error */
      break;
    }
    /* console event */
    if (check_error(lpSelectData, PeekConsoleInput(lpQuery->hFileDescr, &record, 1, &n) == 0))
    {
      break;
    }
    /* check for ASCII keypress only */
    if (record.EventType == KEY_EVENT &&
      record.Event.KeyEvent.bKeyDown &&
      record.Event.KeyEvent.uChar.AsciiChar != 0)
    {
      select_data_result_add(lpSelectData, lpQuery->EMode, lpQuery->lpOrig);
      lpSelectData->EState = SELECT_STATE_SIGNALED;
      break;
    }
    else 
    {
      /* discard everything else and try again */
      if (check_error(lpSelectData, ReadConsoleInput(lpQuery->hFileDescr, &record, 1, &n) == 0))
      {
        break;
      }
    }
  };
}

/* Add a function to monitor console input */
LPSELECTDATA read_console_poll_add (LPSELECTDATA lpSelectData, SELECTMODE EMode, HANDLE hFileDescr, LPVOID lpOrig)
{
  LPSELECTDATA res;

  res = select_data_new(lpSelectData, SELECT_TYPE_CONSOLE_READ);
  res->funcWorker = read_console_poll;
  select_data_query_add(res, SELECT_MODE_READ, hFileDescr, lpOrig);

  return res;
}

/***********************/
/*        Pipe         */
/***********************/

/* Monitor a pipe for input */
void read_pipe_poll (HANDLE hStop, void *_data)
{
  DWORD         event;
  DWORD         n;
  LPSELECTQUERY iterQuery;
  LPSELECTDATA  lpSelectData;
  DWORD         i;

  /* Poll pipe */
  event = 0;
  n = 0;
  lpSelectData = (LPSELECTDATA)_data;

#ifdef DBUG
  dbug_print("Checking data pipe");
#endif
  while (lpSelectData->EState == SELECT_STATE_NONE)
  {
    for (i = 0; i < lpSelectData->nQueriesCount; i++)
    {
      iterQuery = &(lpSelectData->aQueries[i]);
      if (check_error(
            lpSelectData, 
            PeekNamedPipe(
              iterQuery->hFileDescr, 
              NULL, 
              0, 
              NULL, 
              &n, 
              NULL) == 0))
      {
        break;
      };

      if (n > 0)
      {
        lpSelectData->EState = SELECT_STATE_SIGNALED;
        select_data_result_add(lpSelectData, iterQuery->EMode, iterQuery->lpOrig);
      };
    };

    /* Alas, nothing except polling seems to work for pipes.
       Check the state & stop_worker_event every 10 ms 
     */
    if (lpSelectData->EState == SELECT_STATE_NONE)
    {
      event = WaitForSingleObject(hStop, 10);
      if (event == WAIT_OBJECT_0 || check_error(lpSelectData, event == WAIT_FAILED))
      {
        break;
      }
    }
  }
#ifdef DBUG
  dbug_print("Finish checking data on pipe");
#endif
}

/* Add a function to monitor pipe input */
LPSELECTDATA read_pipe_poll_add (LPSELECTDATA lpSelectData, SELECTMODE EMode, HANDLE hFileDescr, LPVOID lpOrig)
{
  LPSELECTDATA res;
  LPSELECTDATA hd;
  
  hd = lpSelectData;
  /* Polling pipe is a non blocking operation by default. This means that each
     worker can handle many pipe. We begin to try to find a worker that is 
     polling pipe, but for which there is under the limit of pipe per worker.
     */
#ifdef DBUG
  dbug_print("Searching an available worker handling pipe");
#endif
  res = select_data_job_search(&hd, SELECT_TYPE_PIPE_READ);
  
  /* Add a new pipe to poll */
  res->funcWorker = read_pipe_poll;
  select_data_query_add(res, EMode, hFileDescr, lpOrig);

  return hd;
}

/***********************/
/*       Socket        */
/***********************/

/* Monitor socket */
void socket_poll (HANDLE hStop, void *_data)
{
  LPSELECTDATA   lpSelectData;
  LPSELECTQUERY  iterQuery;
  HANDLE         aEvents[MAXIMUM_SELECT_OBJECTS];
  DWORD          nEvents;
  long           maskEvents;
  DWORD          i;
  u_long         iMode;

  lpSelectData = (LPSELECTDATA)_data;

  for (nEvents = 0; nEvents < lpSelectData->nQueriesCount; nEvents++)
  {
    iterQuery = &(lpSelectData->aQueries[nEvents]);
    aEvents[nEvents] = CreateEvent(NULL, TRUE, FALSE, NULL);
    maskEvents = 0;
    switch (iterQuery->EMode)
    {
      case SELECT_MODE_READ:
        maskEvents = FD_READ | FD_ACCEPT | FD_CLOSE;
        break;
      case SELECT_MODE_WRITE:
        maskEvents = FD_WRITE | FD_CONNECT | FD_CLOSE;
        break;
      case SELECT_MODE_EXCEPT:
        maskEvents = FD_OOB;
        break;
    }
    check_error(lpSelectData,
        WSAEventSelect(
          (SOCKET)(iterQuery->hFileDescr), 
          aEvents[nEvents], 
          maskEvents) == SOCKET_ERROR);
  }
  
  /* Add stop event */
  aEvents[nEvents]  = hStop;
  nEvents++;

  if (lpSelectData->nError == 0)
  {
    check_error(lpSelectData, 
        WaitForMultipleObjects(
          nEvents, 
          aEvents, 
          FALSE, 
          INFINITE) == WAIT_FAILED);
  };

  if (lpSelectData->nError == 0)
  {
    for (i = 0; i < lpSelectData->nQueriesCount; i++)
    {
      iterQuery = &(lpSelectData->aQueries[i]);
      if (WaitForSingleObject(aEvents[i], 0) == WAIT_OBJECT_0)
      {
#ifdef DBUG
        dbug_print("Socket %d has pending events", (i - 1));
#endif
        if (iterQuery != NULL)
        {
          select_data_result_add(lpSelectData, iterQuery->EMode, iterQuery->lpOrig);
        }
      }
      /* WSAEventSelect() automatically sets socket to nonblocking mode.
         Restore the blocking one. */
      iMode = 0;
      check_error(lpSelectData,
        WSAEventSelect((SOCKET)(iterQuery->hFileDescr), aEvents[i], 0) != 0 ||
        ioctlsocket((SOCKET)(iterQuery->hFileDescr), FIONBIO, &iMode) != 0);

      CloseHandle(aEvents[i]);
      aEvents[i] = INVALID_HANDLE_VALUE;
    }
  }
}

/* Add a function to monitor socket */
LPSELECTDATA socket_poll_add (LPSELECTDATA lpSelectData, SELECTMODE EMode, HANDLE hFileDescr, LPVOID lpOrig)
{
  LPSELECTDATA res;
  LPSELECTDATA hd;
  
  hd = lpSelectData;
  /* Polling socket can be done mulitple handle at the same time. You just
     need one worker to use it. Try to find if there is already a worker
     handling this kind of request.
     */
#ifdef DBUG
  dbug_print("Scanning list of worker to find one that already handle socket");
#endif
  res = select_data_job_search(&hd, SELECT_TYPE_SOCKET);
  
  /* Add a new socket to poll */
  res->funcWorker = socket_poll;
#ifdef DBUG
  dbug_print("Add socket %x to worker", hFileDescr);
#endif
  select_data_query_add(res, EMode, hFileDescr, lpOrig);
#ifdef DBUG
  dbug_print("Socket %x added", hFileDescr);
#endif

  return hd;
}

/***********************/
/*       Static        */
/***********************/

/* Add a static result */
LPSELECTDATA static_poll_add (LPSELECTDATA lpSelectData, SELECTMODE EMode, HANDLE hFileDescr, LPVOID lpOrig)
{
  LPSELECTDATA res;
  LPSELECTDATA hd;
  
  /* Look for an already initialized static element */
  hd = lpSelectData;
  res = select_data_job_search(&hd, SELECT_TYPE_STATIC);
  
  /* Add a new query/result */
  select_data_query_add(res, EMode, hFileDescr, lpOrig);
  select_data_result_add(res, EMode, lpOrig);

  return hd;
}

/********************************/
/* Generic select data handling */
/********************************/

/* Guess handle type */
static SELECTHANDLETYPE get_handle_type(value fd)
{
  DWORD            mode;
  SELECTHANDLETYPE res;

  CAMLparam1(fd);

  mode = 0;
  res = SELECT_HANDLE_NONE;

  if (Descr_kind_val(fd) == KIND_SOCKET)
  {
    res = SELECT_HANDLE_SOCKET;
  }
  else
  {
    switch(GetFileType(Handle_val(fd)))
    {
      case FILE_TYPE_DISK: 
        res = SELECT_HANDLE_DISK;
        break;

      case FILE_TYPE_CHAR: /* character file or a console */
        if (GetConsoleMode(Handle_val(fd), &mode) != 0)
        {
          res = SELECT_HANDLE_CONSOLE;
        }
        else
        {
          res = SELECT_HANDLE_NONE;
        };
        break;

      case FILE_TYPE_PIPE: /* a named or an anonymous pipe (socket already handled) */
        res = SELECT_HANDLE_PIPE;
        break;
    };
  };

  CAMLreturnT(SELECTHANDLETYPE, res);
}

/* Choose what to do with given data */
LPSELECTDATA select_data_dispatch (LPSELECTDATA lpSelectData, SELECTMODE EMode, value fd)
{
  LPSELECTDATA    res;
  HANDLE          hFileDescr;
  void           *lpOrig;
  struct sockaddr sa;
  int             sa_len;
  BOOL            alreadyAdded;

  CAMLparam1(fd);

  res          = lpSelectData;
  hFileDescr   = Handle_val(fd);
  lpOrig       = (void *)fd;
  sa_len       = sizeof(sa);
  alreadyAdded = FALSE;

#ifdef DBUG
  dbug_print("Begin dispatching handle %x", hFileDescr);
#endif

#ifdef DBUG
  dbug_print("Waiting for %d on handle %x", EMode, hFileDescr);
#endif
  
  /* There is only 2 way to have except mode: transmission of OOB data through 
     a socket TCP/IP and through a strange interaction with a TTY.
     With windows, we only consider the TCP/IP except condition
  */
  switch(get_handle_type(fd))
  {
    case SELECT_HANDLE_DISK:
#ifdef DBUG
      dbug_print("Handle %x is a disk handle", hFileDescr);
#endif
      /* Disk is always ready in read/write operation */
      if (EMode == SELECT_MODE_READ || EMode == SELECT_MODE_WRITE)
      {
        res = static_poll_add(res, EMode, hFileDescr, lpOrig);
      };
      break;

    case SELECT_HANDLE_CONSOLE:
#ifdef DBUG
      dbug_print("Handle %x is a console handle", hFileDescr);
#endif
      /* Console is always ready in write operation, need to check for read. */
      if (EMode == SELECT_MODE_READ)
      {
        res = read_console_poll_add(res, EMode, hFileDescr, lpOrig);
      }
      else if (EMode == SELECT_MODE_WRITE)
      {
        res = static_poll_add(res, EMode, hFileDescr, lpOrig);
      };
      break;

    case SELECT_HANDLE_PIPE:
#ifdef DBUG
      dbug_print("Handle %x is a pipe handle", hFileDescr);
#endif
      /* Console is always ready in write operation, need to check for read. */
      if (EMode == SELECT_MODE_READ)
      {
#ifdef DBUG
        dbug_print("Need to check availability of data on pipe");
#endif
        res = read_pipe_poll_add(res, EMode, hFileDescr, lpOrig);
      }
      else if (EMode == SELECT_MODE_WRITE)
      {
#ifdef DBUG
        dbug_print("No need to check availability of data on pipe, write operation always possible");
#endif
        res = static_poll_add(res, EMode, hFileDescr, lpOrig);
      };
      break;

    case SELECT_HANDLE_SOCKET:
#ifdef DBUG
      dbug_print("Handle %x is a socket handle", hFileDescr);
#endif
      if (getsockname((SOCKET)hFileDescr, &sa, &sa_len) == SOCKET_ERROR)
      {
        if (WSAGetLastError() == WSAEINVAL)
        {
          /* Socket is not bound */
#ifdef DBUG
          dbug_print("Socket is not connected");
#endif
          if (EMode == SELECT_MODE_WRITE || EMode == SELECT_MODE_READ)
          {
            res = static_poll_add(res, EMode, hFileDescr, lpOrig);
            alreadyAdded = TRUE;
          }
        }
      }
      if (!alreadyAdded)
      {
        res = socket_poll_add(res, EMode, hFileDescr, lpOrig);
      }
      break;

    default:
#ifdef DBUG
      dbug_print("Handle %x is unknown", hFileDescr);
#endif
      caml_failwith("Unknown handle");
      break;
  };

#ifdef DBUG
  dbug_print("Finish dispatching handle %x", hFileDescr);
#endif

  CAMLreturnT(LPSELECTDATA, res);
}

static DWORD caml_list_length (value lst)
{
  DWORD res;

  CAMLparam1 (lst);
  CAMLlocal1 (l);

  for (res = 0, l = lst; l != Val_int(0); l = Field(l, 1), res++)
  { }

  CAMLreturnT(DWORD, res);
}

#define MAX(a, b) ((a) > (b) ? (a) : (b))

CAMLprim value unix_select(value readfds, value writefds, value exceptfds, value timeout)
{  
  /* Event associated to handle */
  DWORD   nEventsCount;
  DWORD   nEventsMax;
  HANDLE *lpEventsDone;
  
  /* Data for all handles */
  LPSELECTDATA lpSelectData;
  LPSELECTDATA iterSelectData;

  /* Iterator for results */
  LPSELECTRESULT iterResult;

  /* Iterator */
  DWORD i;

  /* Error status */
  DWORD err;

  /* Time to wait */
  DWORD milliseconds;

  /* Is there static select data */
  BOOL  hasStaticData = FALSE;

  /* Wait return */
  DWORD waitRet;

  /* Set of handle */
  SELECTHANDLESET hds;
  DWORD           hdsMax;
  LPHANDLE        hdsData;

  /* Length of each list */
  DWORD readfds_len;
  DWORD writefds_len;
  DWORD exceptfds_len;

  CAMLparam4 (readfds, writefds, exceptfds, timeout);
  CAMLlocal5 (read_list, write_list, except_list, res, l);
  CAMLlocal1 (fd);

#ifdef DBUG
  dbug_print("in select");
#endif

  nEventsCount   = 0;
  nEventsMax     = 0;
  lpEventsDone   = NULL;
  lpSelectData   = NULL;
  iterSelectData = NULL;
  iterResult     = NULL;
  err            = 0;
  hasStaticData  = 0;
  waitRet        = 0;
  readfds_len    = caml_list_length(readfds);
  writefds_len   = caml_list_length(writefds);
  exceptfds_len  = caml_list_length(exceptfds);
  hdsMax         = MAX(readfds_len, MAX(writefds_len, exceptfds_len));

  if (!HeapLock(GetProcessHeap()))
  {
    win32_maperr(GetLastError());
    uerror("select", Nothing);
  }
  hdsData = (HANDLE *)HeapAlloc(
      GetProcessHeap(), 
      0, 
      sizeof(HANDLE) * hdsMax);
  HeapUnlock(GetProcessHeap());

  if (Double_val(timeout) >= 0.0)
  {
    milliseconds = 1000 * Double_val(timeout);
#ifdef DBUG
    dbug_print("Will wait %d ms", milliseconds);
#endif
  }
  else
  {
    milliseconds = INFINITE;
  }


  /* Create list of select data, based on the different list of fd to watch */
#ifdef DBUG
  dbug_print("Dispatch read fd");
#endif
  handle_set_init(&hds, hdsData, hdsMax);
  for (l = readfds; l != Val_int(0); l = Field(l, 1))
  {
    fd = Field(l, 0);
    if (!handle_set_mem(&hds, Handle_val(fd)))
    {
      handle_set_add(&hds, Handle_val(fd));
      lpSelectData = select_data_dispatch(lpSelectData, SELECT_MODE_READ, fd);
    }
    else
    {
#ifdef DBUG
      dbug_print("Discarding handle %x which is already monitor for read", Handle_val(fd));
#endif
    }
  }
  handle_set_reset(&hds);

#ifdef DBUG
  dbug_print("Dispatch write fd");
#endif
  handle_set_init(&hds, hdsData, hdsMax);
  for (l = writefds; l != Val_int(0); l = Field(l, 1))
  {
    fd = Field(l, 0);
    if (!handle_set_mem(&hds, Handle_val(fd)))
    {
      handle_set_add(&hds, Handle_val(fd));
      lpSelectData = select_data_dispatch(lpSelectData, SELECT_MODE_WRITE, fd);
    }
    else
    {
#ifdef DBUG
      dbug_print("Discarding handle %x which is already monitor for write", Handle_val(fd));
#endif
    }
  }
  handle_set_reset(&hds);

#ifdef DBUG
  dbug_print("Dispatch exceptional fd");
#endif
  handle_set_init(&hds, hdsData, hdsMax);
  for (l = exceptfds; l != Val_int(0); l = Field(l, 1))
  {
    fd = Field(l, 0);
    if (!handle_set_mem(&hds, Handle_val(fd)))
    {
      handle_set_add(&hds, Handle_val(fd));
      lpSelectData = select_data_dispatch(lpSelectData, SELECT_MODE_EXCEPT, fd);
    }
    else
    {
#ifdef DBUG
      dbug_print("Discarding handle %x which is already monitor for exceptional", Handle_val(fd));
#endif
    }
  }
  handle_set_reset(&hds);

  /* Building the list of handle to wait for */
#ifdef DBUG
  dbug_print("Building events done array");
#endif
  nEventsMax   = list_length((LPLIST)lpSelectData);
  nEventsCount = 0;
  if (!HeapLock(GetProcessHeap()))
  {
    win32_maperr(GetLastError());
    uerror("select", Nothing);
  }
  lpEventsDone = (HANDLE *)HeapAlloc(GetProcessHeap(), 0, sizeof(HANDLE) * nEventsMax);
  HeapUnlock(GetProcessHeap());

  iterSelectData = lpSelectData;
  while (iterSelectData != NULL)
  {
    /* Check if it is static data. If this is the case, launch everything
     * but don't wait for events. It helps to test if there are events on
     * any other fd (which are not static), knowing that there is at least
     * one result (the static data).
     */
    if (iterSelectData->EType == SELECT_TYPE_STATIC)
    {
      hasStaticData = TRUE;
    };

    /* Execute APC */
    if (iterSelectData->funcWorker != NULL)
    {
      iterSelectData->lpWorker = 
        worker_job_submit(
            iterSelectData->funcWorker, 
            (void *)iterSelectData);
#ifdef DBUG
      dbug_print("Job submitted to worker %x", iterSelectData->lpWorker); 
#endif
      lpEventsDone[nEventsCount] = worker_job_event_done(iterSelectData->lpWorker);
      nEventsCount++;
    };
    iterSelectData = LIST_NEXT(LPSELECTDATA, iterSelectData);
  };

#ifdef DBUG
  dbug_print("Need to watch %d workers", nEventsCount);
#endif

  /* Processing select itself */
  enter_blocking_section();
  /* There are worker started, waiting to be monitored */
  if (nEventsCount > 0)
  {
    /* Waiting for event */
    if (err == 0 && !hasStaticData)
    {
#ifdef DBUG
      dbug_print("Waiting for one select worker to be done");
#endif
      switch (WaitForMultipleObjects(nEventsCount, lpEventsDone, FALSE, milliseconds))
      {
        case WAIT_FAILED:
          err = GetLastError();
          break;

        case WAIT_TIMEOUT:
#ifdef DBUG
          dbug_print("Select timeout");
#endif
          break;

        default:
#ifdef DBUG
          dbug_print("One worker is done");
#endif
          break;
      };
    }

    /* Ordering stop to every worker */
#ifdef DBUG
    dbug_print("Sending stop signal to every select workers");
#endif
    iterSelectData = lpSelectData;
    while (iterSelectData != NULL)
    {
      if (iterSelectData->lpWorker != NULL)
      {
        worker_job_stop(iterSelectData->lpWorker);
      };
      iterSelectData = LIST_NEXT(LPSELECTDATA, iterSelectData);
    };
      
#ifdef DBUG
    dbug_print("Waiting for every select worker to be done");
#endif
    switch (WaitForMultipleObjects(nEventsCount, lpEventsDone, TRUE, INFINITE))
    {
      case WAIT_FAILED:
        err = GetLastError();
        break;

      default:
#ifdef DBUG
        dbug_print("Every worker is done");
#endif
        break;
    }
  }
  /* Nothing to monitor but some time to wait. */
  else if (!hasStaticData)
  {
    Sleep(milliseconds);
  }
  leave_blocking_section();

#ifdef DBUG
  dbug_print("Error status: %d (0 is ok)", err);
#endif
  /* Build results */
  if (err == 0)
  {
#ifdef DBUG
    dbug_print("Building result");
#endif
    read_list = Val_unit; 
    write_list = Val_unit;
    except_list = Val_unit;

    iterSelectData = lpSelectData;
    while (iterSelectData != NULL)
    {
      for (i = 0; i < iterSelectData->nResultsCount; i++)
      {
        iterResult = &(iterSelectData->aResults[i]);
        l = alloc_small(2, 0);
        Store_field(l, 0, (value)iterResult->lpOrig);
        switch (iterResult->EMode)
        {
        case SELECT_MODE_READ:
          Store_field(l, 1, read_list);
          read_list = l;
          break;
        case SELECT_MODE_WRITE:
          Store_field(l, 1, write_list);
          write_list = l;
          break;
        case SELECT_MODE_EXCEPT:
          Store_field(l, 1, except_list);
          except_list = l;
          break;
        }
      }
      /* We try to only process the first error, bypass other errors */
      if (err == 0 && iterSelectData->EState == SELECT_STATE_ERROR)
      {
        err = iterSelectData->nError;
      }
      iterSelectData = LIST_NEXT(LPSELECTDATA, iterSelectData);
    }
  }

  /* Free resources */
#ifdef DBUG
  dbug_print("Free selectdata resources");
#endif
  iterSelectData = lpSelectData;
  while (iterSelectData != NULL)
  {
    lpSelectData = iterSelectData;
    iterSelectData = LIST_NEXT(LPSELECTDATA, iterSelectData);
    select_data_free(lpSelectData);
  }
  lpSelectData = NULL;
  
  /* Free allocated events/handle set array */
#ifdef DBUG
  dbug_print("Free local allocated resources");
#endif
  if (!HeapLock(GetProcessHeap()))
  {
    win32_maperr(GetLastError());
    uerror("select", Nothing);
  }
  HeapFree(GetProcessHeap(), 0, lpEventsDone);
  HeapFree(GetProcessHeap(), 0, hdsData);
  HeapUnlock(GetProcessHeap());

#ifdef DBUG
  dbug_print("Raise error if required");
#endif
  if (err != 0)
  {
    win32_maperr(err);
    uerror("select", Nothing);
  }

#ifdef DBUG
  dbug_print("Build final result");
#endif
  res = alloc_small(3, 0);
  Store_field(res, 0, read_list);
  Store_field(res, 1, write_list);
  Store_field(res, 2, except_list);

#ifdef DBUG
  dbug_print("out select");
#endif

  CAMLreturn(res);
}
