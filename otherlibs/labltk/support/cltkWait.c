/***********************************************************************/
/*                                                                     */
/*                 MLTk, Tcl/Tk interface of OCaml                     */
/*                                                                     */
/*    Francois Rouaix, Francois Pessaux, Jun Furuse and Pierre Weis    */
/*               projet Cristal, INRIA Rocquencourt                    */
/*            Jacques Garrigue, Kyoto University RIMS                  */
/*                                                                     */
/*  Copyright 2002 Institut National de Recherche en Informatique et   */
/*  en Automatique and Kyoto University.  All rights reserved.         */
/*  This file is distributed under the terms of the GNU Library        */
/*  General Public License, with the special exception on linking      */
/*  described in file LICENSE found in the OCaml source tree.          */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#include <tcl.h>
#include <tk.h>
#include <mlvalues.h>
#include <memory.h>
#include <callback.h>
#include "camltk.h"

/* The following are replacements for
    tkwait visibility
    tkwait window
   in the case where we use threads (tkwait internally calls an event loop,
   and thus prevents thread scheduling from taking place).

   Instead, one should set up a callback, wait for a signal, and signal
   from inside the callback
*/

static void             WaitVisibilityProc _ANSI_ARGS_((ClientData clientData,
                            XEvent *eventPtr));
static void             WaitWindowProc _ANSI_ARGS_((ClientData clientData,
                            XEvent *eventPtr));

/* For the other handlers, we need a bit more data */
struct WinCBData {
  int cbid;
  Tk_Window win;
};

static void WaitVisibilityProc(clientData, eventPtr)
    ClientData clientData;
    XEvent *eventPtr;           /* Information about event (not used). */
{
  struct WinCBData *vis = clientData;
  value cbid = Val_int(vis->cbid);

  Tk_DeleteEventHandler(vis->win, VisibilityChangeMask,
            WaitVisibilityProc, clientData);

  stat_free((char *)vis);
  callback2(*handler_code,cbid,Val_int(0));
}

/* Sets up a callback upon Visibility of a window */
CAMLprim value camltk_wait_vis(value win, value cbid)
{
  struct WinCBData *vis =
    (struct WinCBData *)stat_alloc(sizeof(struct WinCBData));
  vis->win = Tk_NameToWindow(cltclinterp, String_val(win), cltk_mainWindow);
  if (vis -> win == NULL) {
    stat_free((char *)vis);
    tk_error(Tcl_GetStringResult(cltclinterp));
  };
  vis->cbid = Int_val(cbid);
  Tk_CreateEventHandler(vis->win, VisibilityChangeMask,
                        WaitVisibilityProc, (ClientData) vis);
  return Val_unit;
}

static void WaitWindowProc(ClientData clientData, XEvent *eventPtr)
{
  if (eventPtr->type == DestroyNotify) {
    struct WinCBData *vis = clientData;
    value cbid = Val_int(vis->cbid);
    stat_free((char *)clientData);
    /* The handler is destroyed by Tk itself */
    callback2(*handler_code,cbid,Val_int(0));
  }
}

/* Sets up a callback upon window destruction */
CAMLprim value camltk_wait_des(value win, value cbid)
{
  struct WinCBData *vis =
    (struct WinCBData *)stat_alloc(sizeof(struct WinCBData));
  vis->win = Tk_NameToWindow(cltclinterp, String_val(win), cltk_mainWindow);
  if (vis -> win == NULL) {
    stat_free((char *)vis);
    tk_error(Tcl_GetStringResult(cltclinterp));
  };
  vis->cbid = Int_val(cbid);
  Tk_CreateEventHandler(vis->win, StructureNotifyMask,
                        WaitWindowProc, (ClientData) vis);
  return Val_unit;
}
