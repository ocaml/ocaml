#ifdef _WIN32
#include <wtypes.h>
#include <winbase.h>
#include <winsock.h>
#endif
#include <tcl.h>
#include <tk.h>
#include <caml/mlvalues.h>
#include "camltk.h"

/*
 * File descriptor callbacks
 */

void FileProc(ClientData clientdata, int mask)
{
  callback2(*handler_code,Val_int(clientdata),Val_int(0));
}

/* Map Unix.file_descr values to Tcl file handles */

#ifndef _WIN32

/* Unix system */

#if TCL_MAJOR_VERSION >= 8
#define tcl_filehandle(fd) Int_val(fd)
#define Tcl_File int
#define Tcl_FreeFile(fd)
#else
static Tcl_File tcl_filehandle(value fd)
{
  return Tcl_GetFile((ClientData)Long_val(fd), TCL_UNIX_FD);
}
#endif

#else

/* Windows */

#define Handle_val(v) (*((HANDLE *)(v)))

static Tcl_File tcl_filehandle(value fd)
{
  HANDLE h = Handle_val(fd);
  int type;
  int optval, optsize;

  optsize = sizeof(optval);
  if (getsockopt((SOCKET) h, SOL_SOCKET, SO_TYPE, &optval, &optsize) == 0)
    type = TCL_WIN_SOCKET;
  else
    switch (GetFileType(h)) {
    case FILE_TYPE_CHAR:
      type = TCL_WIN_CONSOLE;
    case FILE_TYPE_PIPE:
      type = TCL_WIN_PIPE;
    case FILE_TYPE_DISK:
    default:                    /* use WIN_FILE for unknown handles */
      type = TCL_WIN_FILE;
    }
  return Tcl_GetFile(h, type);
}

#endif

value camltk_add_file_input(fd, cbid)    /* ML */
     value fd;
     value cbid;
{
  CheckInit();
  Tcl_CreateFileHandler(tcl_filehandle(fd), TCL_READABLE, 
		       FileProc, (ClientData)(Long_val(cbid)));
  return Val_unit;
}

/* We have to free the Tcl handle when we are finished using it (Tcl
 * asks us to, and moreover it is probably dangerous to keep the same
 * handle over two allocations of the same fd by the kernel). 
 * But we don't know when we are finished with the fd, so we free it
 * in rem_file (it doesn't close the fd anyway). For fds for which we
 * repeatedly add/rem, this will cause some overhead.
 */
value camltk_rem_file_input(fd) /* ML */
     value fd;
{
  Tcl_File fh = tcl_filehandle(fd);
  Tcl_DeleteFileHandler(fh);
  Tcl_FreeFile(fh);
  return Val_unit;
}

value camltk_add_file_output(fd, cbid)    /* ML */
     value fd;
     value cbid;
{
  CheckInit();
  Tcl_CreateFileHandler(tcl_filehandle(fd), TCL_WRITABLE, 
		       FileProc, (ClientData) (Long_val(cbid)));
  return Val_unit;
}

value camltk_rem_file_output(fd) /* ML */
     value fd;
{
  Tcl_File fh = tcl_filehandle(fd);
  Tcl_DeleteFileHandler(fh);
  Tcl_FreeFile(fh);
  return Val_unit;
}

