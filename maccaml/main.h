/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*             Damien Doligez, projet Para, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1997 Institut National de Recherche en Informatique et   */
/*  en Automatique.  Distributed only by permission.                   */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#include <limits.h>
#include <signal.h>
#include <stdio.h>
#include <string.h>

#include <AERegistry.h>
#include <AppleEvents.h>
#include <Controls.h>
#include <Devices.h>
#include <Dialogs.h>
#include <DiskInit.h>
#include <Drag.h>
#include <Finder.h>
#include <FixMath.h>
#include <Folders.h>
#include <Fonts.h>
#include <Gestalt.h>
#include <LowMem.h>
#include <MacWindows.h>
#include <Menus.h>
#include <Power.h>
#include <Processes.h>
#include <QDOffscreen.h>
#include <QuickDraw.h>
#include <Resources.h>
#include <Scrap.h>
#include <Script.h>
#include <SegLoad.h>
#include <Sound.h>
#include <StandardFile.h>
#include <Strings.h>
#include <TextUtils.h>
#include <ToolUtils.h>
#include <Types.h>

#include ":WASTE-1.2:WASTE C/C++ Headers:WASTE.h"

#include "constants.h"

#if DEBUG
#define Assert(cond) if (!(cond)) assert_failure (#cond, __FILE__, __LINE__)
#else
#define Assert(cond)
#endif

/* Vertical and Horizontal */
#define V 0
#define H 1

typedef struct WStatus {
  int kind;
  short datarefnum;         /* window's file (data fork) */
  short resrefnum;          /* window's file (resource fork) or -1 */
  unsigned long basemodcount;
  struct menuflags {
    unsigned int save : 1;
    unsigned int save_as : 1;
    unsigned int revert : 1;
    unsigned int page_setup : 1;
    unsigned int print : 1;
    unsigned int cut : 1;
    unsigned int copy : 1;
    unsigned int paste : 1;
    unsigned int clear : 1;
    unsigned int select_all : 1;
    unsigned int find : 1;
    unsigned int replace : 1;
  } menuflags;
  long line_height;
  ControlHandle scrollbars [2];
  LongRect viewrect, destrect;     /* view and dest for the graphics window */
  WEHandle we;
} **WStatusH;

typedef enum { closingWindow = 0, closingApp } ClosingOption;
typedef enum { noWait = 0, waitMove, waitEvent } WaitEventOption;

#define PREF_VERSION 2
/* Increment PREF_VERSION at each change in struct prefs. */
struct prefs {
  long version;
  int asksavetop;
  Rect toppos;
  Rect graphpos;
  Rect clippos;
  TextStyle text;
  TextStyle unread;
  TextStyle input;
  TextStyle output;
  TextStyle errors;
};

/* aboutbox.c */
void OpenAboutBox (void);
void CloseAboutBox (WindowPtr w);
void DrawAboutIcon (void);

/* appleevents.c */
OSErr InstallAEHandlers (void);

/* clipboard.c */
void ClipShow (void);
void ClipClose (void);
void ClipChanged (void);

/* drag.c */
OSErr InstallDragHandlers (void);
OSErr RemoveDragHandlers (void);

/* errors.c */
void assert_failure (char *condition, char *file, int line);
void XXX (void);
void ErrorAlert (short msg1, Str255 bufmsg2, short msg3, OSErr err);
void ErrorAlertCantOpen (Str255 filename, OSErr err);
void ErrorAlertGeneric (OSErr err);
OSErr InitialiseErrors (void);

/* events.c */
extern UInt32 evtSleep;
void GetAndProcessEvents (WaitEventOption wait, short oldx, short oldy);
OSErr InitialiseEvents (void);
extern AEIdleUPP ProcessEventUPP;

/* files.c */
OSErr FileDoClose (WindowPtr w, ClosingOption close);
void FileDoGetOpen (void);
void FileNew (void);
OSErr FileOpen (FSSpec *filespec);
void FileRevert (WindowPtr w);
OSErr FileDoSave (WindowPtr w, int saveasflag);

/* glue.c */
extern int caml_at_work;
void Caml_working (int newstate);
void GlueInterrupt (void);
OSErr launch_caml_main (void);

/* graph.c */
void GraphGotEvent (EventRecord *evt);
void GraphNewSizePos (void);
void GraphScroll (long dx, long dy);
void GraphUpdate (void);

/* lcontrols.c */
OSErr LCAttach( ControlRef );
void LCDetach( ControlRef );
void LCSetValue( ControlRef, long );
void LCSetMin( ControlRef, long );
void LCSetMax( ControlRef, long );
long LCGetValue( ControlRef );
long LCGetMin( ControlRef );
long LCGetMax( ControlRef );
void LCSynch( ControlRef );

/* main.c */
extern int gHasDragAndDrop;
extern int gHasPowerManager;
extern int quit_requested;
extern int launch_toplevel_requested;
void ExitApplication (void);

/* memory.c */
OSErr AllocHandle (Size size, Handle *result);

/* menus.c */
void DoMenuChoice (long item, EventModifiers mods);
OSErr DoQuit (void);
OSErr InitialiseMenus (void);
OSErr MenuWinAdd (WindowPtr w);
void MenuWinRemove (WindowPtr w);
void UpdateMenus (void);

/* misc.c */
void LocalToGlobalRect (Rect *r);

/* modalfilter.c */
extern short modalkeys;
extern ModalFilterUPP myModalFilterUPP;
OSErr InitialiseModalFilter (void);

/* prefs.c */
extern struct prefs prefs;
void ReadPrefs (void);
void WritePrefs (void);

/* scroll.c */
extern WEScrollUPP scrollFollowUPP;
void AdjustScrollBars (WindowPtr w);
OSErr InitialiseScroll (void);
int ScrollAtEnd (WindowPtr w);
void ScrollCalcText (WindowPtr w, Rect *r);
void ScrollCalcGraph (WindowPtr w, Rect *r);
void ScrollDoClick (WindowPtr w, Point where, EventModifiers mods);
void ScrollNewSize (WindowPtr w);
void ScrollToEnd (WindowPtr w);

/* windows.c */
extern WindowPtr winToplevel;
extern WindowPtr winGraphics;
extern long wintopfrontier;
OSErr InitialiseWindows (void);
void WinActivateDeactivate (int activate, WindowPtr w);
void WinAdvanceTopFrontier (long length);
OSErr WinAllocStatus (WindowPtr w);
void WinCloseGraphics (void);
void WinCloseToplevel (void);
void WinDoContentClick (EventRecord *e, WindowPtr w);
OSErr WinDoClose (ClosingOption closing, WindowPtr w);
void WinDoDrag (Point where, WindowPtr w);
void WinDoGrow (Point where, WindowPtr w);
void WinDoIdle (WindowPtr w);
void WinDoKey (WindowPtr w, short chr, EventRecord *e);
void WinDoZoom (WindowPtr w, short partCode);
WStatusH WinGetStatus (WindowPtr w);
WEHandle WinGetWE (WindowPtr w);
int WinGetKind (WindowPtr w);
WindowPtr WinOpenDocument (StringPtr title);
OSErr WinOpenGraphics (long width, long height);
OSErr WinOpenToplevel (void);
void WinClipboardStdState (Rect *r);
void WinGraphicsStdState (Rect *r);
void WinToplevelStdState (Rect *r);
void WinUpdate (WindowPtr w);
void WinUpdateStatus (WindowPtr w);
