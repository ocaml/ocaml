/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*  Developed by Jacob Navia.                                          */
/*  Copyright 2001 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

/*------------------------------------------------------------------------
 Module:        D:\lcc\inria\inria.h
 Author:        Jacob
 Project:
 State:
 Creation Date: June 2001
 Description:   The user interface works as follows:
                1: At startup it will look for the path to the
                ocaml interpreter in the registry using the
                key HKEY_CURRENT_USER\SOFTWARE\ocaml. If not
                found will prompt the user.
                2: It will start the ocaml interpreter with
                its standard output and standard input
                connected to two pipes in a dedicated thread.
                3: It will open a window containing an edit
                field. The output from the interpreter will be
                shown in the edit field, and the input of the
                user in the edit field will be sent to the
                interpreter when the user types return.
                4: Line editing is provided by moving to the
                desired line with the arrows, then pressing
                return; If we aren't in the last input line,
                the input will be copied to the last line and
                sent to the interpreter.
                5: The GUI ensures that when we exit the ocaml
                interpreter is stopped by sending the
                character string "#quit;;\nCtrl-Z"
                6: A history of all lines sent to the interpreter
                is maintained in a simple linked list. The
                History dialog box shows that, and allows the
                user to choose a given input line.
                7: Memory limits. The edit buffer can be of an
                arbitrary length, i.e. maybe 7-8MB or more,
                there are no fixed limits. The History list
                will always grow too, so memory consumption
                could be "high" after several days of
                uninterrupted typing at the keyboard. For that
                cases it is recommended to stop the GUI and
                get some sleep...
                9: The GUI will start a timer, looking 4 times a
                second if the interpreter has written
                something in the pipe. This is enough for most
                applications.
------------------------------------------------------------------------*/
#ifndef _INRIA_H_
#define _INRIA_H_

#include <windows.h>
#include "editbuffer.h"
#include "history.h"

#if _MSC_VER <= 1200 && !defined(__MINGW32__)
#define GetWindowLongPtr GetWindowLong
#define SetWindowLongPtr SetWindowLong
#define DWLP_USER DWL_USER
#define GWLP_WNDPROC GWL_WNDPROC
#define LONG_PTR DWORD
#endif

// In this structure should go eventually all global variables scattered
// through the program.
typedef struct _programParams {
        HFONT hFont;                                    // The handle of the current font
        COLORREF TextColor;                             // The text color
        char CurrentWorkingDir[MAX_PATH];// The current directory
} PROGRAM_PARAMS;

//**************** Global variables ***********************
extern PROGRAM_PARAMS ProgramParams;

extern COLORREF BackColor;                      // The background color
extern HBRUSH BackgroundBrush;          // A brush built with the background color
extern char LibDir[];                           // The lib directory
extern char OcamlPath[];                        // The Path to ocaml.exe
extern HANDLE hInst;                            // The instance handle for this application
extern HWND hwndSession;                        // The current session window handle
extern LOGFONT CurrentFont;                     // The current font characteristics
extern HWND hwndMain,hwndMDIClient; // Window handles of frame and mdi window

// ***************** Function prototypes ******************
int WriteToPipe(char *data);            // Writes to the pipe
int ReadFromPipe(char *data,int len);// Reads from the pipe
int AskYesOrNo(char *msg);                      //Ditto!
int BrowseForFile(char *fname,char *path);
void GotoEOF(void);                                     // Positions the cursor at the end of the text
void ShowDbgMsg(char *msg);                     // Shows an error message
void HandleCommand(HWND hwnd, WPARAM wParam,LPARAM lParam);
int GetOcamlPath(void);                         // Finds where ocaml.exe is
void ForceRepaint(void);                        // Ditto.
void AddLineToControl(char *buf);
void AddStringToControl(char* buf);
char *GetHistoryLine(int n);            // Gets the nth history line base 1.
int StartOcaml(void);
void InterruptOcaml(void);
int ResetText(void);
BOOL SendingFullCommand(void);
void RewriteCurrentEditBuffer(void);
void RefreshCurrentEditBuffer(void);

// **************** User defined window messages *************
#define WM_NEWLINE		(WM_USER+6000)
#define WM_TIMERTICK	(WM_USER+6001)
#define WM_QUITOCAML	(WM_USER+6002)
#define WM_SYNTAXERROR	(WM_USER+6003)
#define WM_UNBOUNDVAL	(WM_USER+6004)
#define WM_ILLEGALCHAR	(WM_USER+6005)

// ********************** Structures ***********************
typedef struct tagPosition {
        int line;
        int col;
} POSITION;

extern void *SafeMalloc(int);
extern StatementHistory *History; // The root of the history lines
extern StatementHistory *HistoryTail; // The tail of the history lines
extern EditBuffer *CurrentEditBuffer; // current edit buffer

#define IDEDITCONTROL 15432
#endif
