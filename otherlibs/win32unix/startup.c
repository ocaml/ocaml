/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*  Xavier Leroy and Pascal Cuoq, projet Cristal, INRIA Rocquencourt   */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  Automatique.  Distributed only by permission.                      */
/*                                                                     */
/***********************************************************************/

#include <stdio.h>
#include <fcntl.h>
#include <stdlib.h>
#include <mlvalues.h>
#include <winsock.h>

value val_process_id;

value win_startup(unit)         /* ML */
     value unit;
{
  WSADATA wsaData;
  int i;
  HANDLE h;

  (void) WSAStartup(MAKEWORD(2, 0), &wsaData);
  DuplicateHandle(GetCurrentProcess(), GetCurrentProcess(),
                  GetCurrentProcess(), &h, 0, TRUE,
                  DUPLICATE_SAME_ACCESS);
  val_process_id = Val_int(h);

  return Val_unit;
}

value win_cleanup(unit)         /* ML */
     value unit;
{
  (void) WSACleanup();
  return Val_unit;
}

static int std_handles[3] = {
  STD_INPUT_HANDLE, STD_OUTPUT_HANDLE, STD_ERROR_HANDLE
};

value win_stdhandle(value nhandle) /* ML */
{
  return win_alloc_handle(GetStdHandle(std_handles[Int_val(nhandle)]));
}
