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

/*@@ Wedit generated application. Written Sat Jun 02 18:22:38 2001
 @@header: D:\lcc\inria\inriares.h
 @@resources: D:\lcc\inria\inria.rc
 Do not edit outside the indicated areas */
/*<---------------------------------------------------------------------->*/
/*<---------------------------------------------------------------------->*/
#include <windows.h>
#include <windowsx.h>
#include <commctrl.h>
#include <string.h>
#include <direct.h>
#include <Richedit.h>
#include "inriares.h"
#include "inria.h"
int EditControls = IDEDITCONTROL;
static WNDPROC lpEProc;
static char lineBuffer[1024*32];
int ResetText(void);
int ReadToLineBuffer(void);
int AddLineBuffer(void);
static int busy;
static DWORD TimerId;
POSITION LastPromptPosition;
char LibDir[512];
char OcamlPath[512];
HBRUSH BackgroundBrush;
COLORREF BackColor = RGB(255,255,255);
PROGRAM_PARAMS ProgramParams;
HISTORYLINE *History;
/*<----------------- global variables --------------------------------------->*/
HANDLE hInst;           // Instance handle
HWND hwndMain;          //Main window handle
HWND hwndSession;
HWND hwndMDIClient;             //Mdi client window handle
static LRESULT CALLBACK MainWndProc(HWND hwnd,UINT msg,WPARAM wParam,LPARAM lParam);
static LRESULT CALLBACK MdiChildWndProc(HWND hwnd,UINT msg,WPARAM wParam,LPARAM lParam);
PROCESS_INFORMATION pi;
HWND  hWndStatusbar;

/*------------------------------------------------------------------------
 Procedure:     UpdateStatusBar ID:1
 Purpose:       Updates the statusbar control with the appropiate
                text
 Input:         lpszStatusString: Charactar string that will be shown
                partNumber: index of the status bar part number.
                displayFlags: Decoration flags
 Output:        none
 Errors:        none

------------------------------------------------------------------------*/
void UpdateStatusBar(LPSTR lpszStatusString, WORD partNumber, WORD displayFlags)
{
    SendMessage(hWndStatusbar,
                SB_SETTEXT,
                partNumber | displayFlags,
                (LPARAM)lpszStatusString);
}


/*------------------------------------------------------------------------
 Procedure:     MsgMenuSelect ID:1
 Purpose:       Shows in the status bar a descriptive explaation of
                the purpose of each menu item.The message
                WM_MENUSELECT is sent when the user starts browsing
                the menu for each menu item where the mouse passes.
 Input:         Standard windows.
 Output:        The string from the resources string table is shown
 Errors:        If the string is not found nothing will be shown.
------------------------------------------------------------------------*/
LRESULT MsgMenuSelect(HWND hwnd, UINT uMessage, WPARAM wparam, LPARAM lparam)
{
    static char szBuffer[256];
    UINT   nStringID = 0;
    UINT   fuFlags = GET_WM_MENUSELECT_FLAGS(wparam, lparam) & 0xffff;
    UINT   uCmd    = GET_WM_MENUSELECT_CMD(wparam, lparam);
    HMENU  hMenu   = GET_WM_MENUSELECT_HMENU(wparam, lparam);

    szBuffer[0] = 0;                            // First reset the buffer
    if (fuFlags == 0xffff && hMenu == NULL)     // Menu has been closed
        nStringID = 0;

    else if (fuFlags & MFT_SEPARATOR)           // Ignore separators
        nStringID = 0;

    else if (fuFlags & MF_POPUP)                // Popup menu
    {
        if (fuFlags & MF_SYSMENU)               // System menu
            nStringID = IDS_SYSMENU;
        else
            // Get string ID for popup menu from idPopup array.
            nStringID = 0;
    }  // for MF_POPUP
    else                                        // Must be a command item
        nStringID = uCmd;                       // String ID == Command ID

    // Load the string if we have an ID
    if (0 != nStringID)
        LoadString(hInst, nStringID, szBuffer, sizeof(szBuffer));
    // Finally... send the string to the status bar
    UpdateStatusBar(szBuffer, 0, 0);
    return 0;
}

/*------------------------------------------------------------------------
 Procedure:     TimerProc ID:1
 Purpose:       This procedure will be called by windows about 4
                times a second. It will just send a message to the
                mdi child window to look at the pipe.
 Input:
 Output:
 Errors:
------------------------------------------------------------------------*/
static VOID CALLBACK TimerProc(HWND hwnd, UINT uMsg, UINT idEvent, DWORD dwTime)
{
   SendMessage(hwndSession, WM_TIMERTICK, 0, 0);
}

/*------------------------------------------------------------------------
 Procedure:     InitializeStatusBar ID:1
 Purpose:       Initialize the status bar
 Input:         hwndParent: the parent window
                nrOfParts: The status bar can contain more than one
                part. What is difficult, is to figure out how this
                should be drawn. So, for the time being only one is
                being used...
 Output:        The status bar is created
 Errors:
------------------------------------------------------------------------*/
void InitializeStatusBar(HWND hwndParent,int nrOfParts)
{
    const int cSpaceInBetween = 8;
    int   ptArray[40];   // Array defining the number of parts/sections
    RECT  rect;
    HDC   hDC;

   /* * Fill in the ptArray...  */

    hDC = GetDC(hwndParent);
    GetClientRect(hwndParent, &rect);

    ptArray[nrOfParts-1] = rect.right;
    //---TODO--- Add code to calculate the size of each part of the status
    // bar here.

    ReleaseDC(hwndParent, hDC);
    SendMessage(hWndStatusbar,
                SB_SETPARTS,
                nrOfParts,
                (LPARAM)(LPINT)ptArray);

    UpdateStatusBar("Ready", 0, 0);
}


/*------------------------------------------------------------------------
 Procedure:     CreateSBar ID:1
 Purpose:       Calls CreateStatusWindow to create the status bar
 Input:         hwndParent: the parent window
                initial text: the initial contents of the status bar
 Output:
 Errors:
------------------------------------------------------------------------*/
static BOOL CreateSBar(HWND hwndParent,char *initialText,int nrOfParts)
{
    hWndStatusbar = CreateStatusWindow(WS_CHILD | WS_VISIBLE | WS_BORDER|SBARS_SIZEGRIP,
                                       initialText,
                                       hwndParent,
                                       IDM_STATUSBAR);
    if(hWndStatusbar)
    {
        InitializeStatusBar(hwndParent,nrOfParts);
        return TRUE;
    }

    return FALSE;
}
/*------------------------------------------------------------------------
 Procedure:     InitApplication ID:1
 Purpose:       Registers two window classes: the "inria" window
                class with the main window, and the mdi child
                window's window class.
 Input:
 Output:
 Errors:
------------------------------------------------------------------------*/
static BOOL InitApplication(void)
{
        WNDCLASS wc;

        memset(&wc,0,sizeof(WNDCLASS));
        wc.style = CS_HREDRAW|CS_VREDRAW |CS_DBLCLKS ;
        wc.lpfnWndProc = (WNDPROC)MainWndProc;
        wc.hInstance = hInst;
        wc.hbrBackground = (HBRUSH)(COLOR_WINDOW+1);
        wc.lpszClassName = "inriaWndClass";
        wc.lpszMenuName = MAKEINTRESOURCE(IDMAINMENU);
        wc.hCursor = LoadCursor(NULL,IDC_ARROW);
        wc.hIcon = LoadIcon(hInst,MAKEINTRESOURCE(OCAML_ICON));
        if (!RegisterClass(&wc))
                return 0;
        wc.style         = 0;
        wc.lpfnWndProc   = (WNDPROC)MdiChildWndProc;
        wc.cbClsExtra    = 0;
        wc.cbWndExtra    = 20;
        wc.hInstance     = hInst;                      // Owner of this class
        wc.hIcon         = LoadIcon(hInst, MAKEINTRESOURCE(OCAML_ICON));
        wc.hCursor       = LoadCursor(NULL, IDC_ARROW);
        wc.hbrBackground = (HBRUSH)(COLOR_WINDOW + 1); // Default color
        wc.lpszMenuName  = NULL;
        wc.lpszClassName = "MdiChildWndClass";
        if (!RegisterClass((LPWNDCLASS)&wc))
                return FALSE;
        return 1;
}

/*------------------------------------------------------------------------
 Procedure:     CreateinriaWndClassWnd ID:1
 Purpose:       Creates the main window
 Input:
 Output:
 Errors:
------------------------------------------------------------------------*/
HWND CreateinriaWndClassWnd(void)
{
        return CreateWindow("inriaWndClass","Ocaml",
                WS_MINIMIZEBOX|WS_VISIBLE|WS_CLIPSIBLINGS|WS_CLIPCHILDREN|WS_MAXIMIZEBOX|WS_CAPTION|WS_BORDER|WS_SYSMENU|WS_THICKFRAME,
                CW_USEDEFAULT,0,CW_USEDEFAULT,0,
                NULL,
                NULL,
                hInst,
                NULL);
}

/*------------------------------------------------------------------------
 Procedure:     MDICmdFileNew ID:1
 Purpose:       Creates a new session window. Note that multiple
                windows with multiple sessions are possible.
 Input:
 Output:
 Errors:
------------------------------------------------------------------------*/
static HWND MDICmdFileNew(char *title, int show)
{
        HWND  hwndChild;
        char  rgch[150];
        static int cUntitled;
        MDICREATESTRUCT mcs;

        if (title == NULL)
                wsprintf(rgch,"Session%d", cUntitled++);
        else {
                strncpy(rgch,title,149);
                rgch[149] = 0;
        }

        // Create the MDI child window

        mcs.szClass = "MdiChildWndClass";      // window class name
        mcs.szTitle = rgch;             // window title
        mcs.hOwner  = hInst;            // owner
        mcs.x       = CW_USEDEFAULT;    // x position
        mcs.y       = CW_USEDEFAULT;    // y position
        mcs.cx      = CW_USEDEFAULT;    // width
        mcs.cy      = CW_USEDEFAULT;    // height
        mcs.style   = 0;                // window style
        mcs.lParam  = 0;                // lparam

        hwndChild = (HWND) SendMessage(hwndMDIClient,
                                       WM_MDICREATE,
                                       0,
                                       (LPARAM)(LPMDICREATESTRUCT) &mcs);

        if (hwndChild != NULL && show)
                ShowWindow(hwndChild, SW_SHOW);

        return hwndChild;
}
static HWND CreateMdiClient(HWND hwndparent)
{
    CLIENTCREATESTRUCT ccs = {0};
    HWND hwndMDIClient;
    int icount = GetMenuItemCount(GetMenu(hwndparent));

    // Find window menu where children will be listed
    ccs.hWindowMenu  = GetSubMenu(GetMenu(hwndparent), icount-2);
    ccs.idFirstChild = IDM_WINDOWCHILD;

    // Create the MDI client filling the client area
    hwndMDIClient = CreateWindow("mdiclient",
                                 NULL,
                                 WS_CHILD | WS_CLIPCHILDREN | WS_VSCROLL |
                                 WS_HSCROLL,
                                 0, 0, 0, 0,
                                 hwndparent,
                                 (HMENU)0xCAC,
                                 hInst,
                                 (LPVOID)&ccs);

    ShowWindow(hwndMDIClient, SW_SHOW);

    return hwndMDIClient;
}

void GotoEOF(void)
{
        HWND hEdit = (HWND)GetWindowLong(hwndSession,DWL_USER);
        int linesCount = SendMessage(hEdit,EM_GETLINECOUNT,0,0);
        int lineindex = SendMessage(hEdit,EM_LINEINDEX,linesCount-1,0);
        int lastLineLength = SendMessage(hEdit,EM_LINELENGTH,linesCount-1,0);

        lineindex += lastLineLength;
        SendMessage(hEdit,EM_SETSEL,lineindex,lineindex);
}

int GetCurLineIndex(HWND hEdit)
{
        return SendMessage(hEdit,EM_LINEFROMCHAR,(WPARAM)-1,0);
}

int GetNumberOfLines(HWND hEdit)
{
        return SendMessage(hEdit,EM_GETLINECOUNT,0,0);
}

static int GetWordUnderCursor(HWND hwndEditControl,char *buf,int len)
{
    char *line,*p,*pstart,*pend;
    int lineidx,start,end,length,offset,cursorpos,startingChar;

    SendMessage(hwndEditControl,EM_GETSEL,(WPARAM)&start,(LPARAM)&end);
    lineidx = SendMessage(hwndEditControl,EM_EXLINEFROMCHAR,0,start);
    startingChar = SendMessage(hwndEditControl,EM_LINEINDEX,lineidx,0);
    start -= startingChar;
    end -= startingChar;
        lineidx = SendMessage(hwndEditControl,EM_LINEFROMCHAR,start,0);
    length = SendMessage(hwndEditControl,EM_LINELENGTH,lineidx,0);
    offset = SendMessage(hwndEditControl,EM_LINEINDEX,lineidx,0);
    line = SafeMalloc(length+1);
    memset(line,0,length+1);
    *(unsigned short *)line = length;
    SendMessage(hwndEditControl,EM_GETLINE,lineidx,(LPARAM)line);
    cursorpos = start-offset;
    p = line + cursorpos;
    pstart = p;
    while (*pstart
            && *pstart != ' '
            && *pstart != '\t'
            && *pstart != '('
            && pstart > line)
            pstart--;
    pend = p;
    while (*pend
            && *pend != ' '
            && *pend != '\t'
            && *pend != '('
            && pend < line + length)
            pend++;
    if (*pstart == ' ' || *pstart == '\t')
            pstart++;
    if (*pend == ' ' || *pend == '\t')
            pend--;
    memcpy(buf,pstart,1+pend-pstart);
    buf[pend-pstart] = 0;
        free(line);
    return 1;
}

void DoHelp(HWND hwnd)
{
    char word[256];
    GetWordUnderCursor(hwnd,word,sizeof(word));
        MessageBox(NULL,word,"Aide pour:",MB_OK);
}


static LRESULT CALLBACK SubClassEdit(HWND hwnd, UINT msg, WPARAM mp1, LPARAM mp2)
{
        LRESULT r;
        int postit=0,nl;
        if (msg == WM_CHAR && mp1 == '\r') {
                if (!busy) {
                        CallWindowProc(lpEProc,hwnd,WM_KEYDOWN,VK_END,1);
                        CallWindowProc(lpEProc,hwnd,WM_KEYUP,VK_END,1);
                        r =  GetCurLineIndex(hwnd);
                        nl = GetNumberOfLines(hwnd);
                        if (r != nl-1) {
                                PostMessage(GetParent(hwnd),WM_NEWLINE,0,0);
                                return 0;
                        }
                        postit = 1;
                }

        }
        else if (msg == WM_KEYDOWN && mp1 == VK_F1) {
                DoHelp(hwnd);
        }
        r = CallWindowProc(lpEProc, hwnd, msg, mp1, mp2);
        if (postit)
                PostMessage(GetParent(hwnd),WM_NEWLINE,0,0);
        return r;
}

static void SubClassEditField(HWND hwnd)
{
        if (lpEProc == NULL) {
                lpEProc = (WNDPROC) GetWindowLong(hwnd, GWL_WNDPROC);
        }
        SetWindowLong(hwnd, GWL_WNDPROC, (DWORD) SubClassEdit);
}

void AddToHistory(char *text)
{
        HISTORYLINE *newLine;

        while (*text == ' ')
                text++; // skip leading blanks
        if (*text == 0)
                return;
        if (History && !strstr(History->Text,";;")) {
                char *p = History->Text;
                int len = strlen(p)+strlen(text) + 1 + 1; // space and zero terminator
                History->Text = SafeMalloc(len);
                strcpy(History->Text,p);
                strcat(History->Text," ");
                strcat(History->Text,text);
                free(p);
                return;
        }
        newLine = SafeMalloc(sizeof(HISTORYLINE));
        newLine->Next = History;
        newLine->Text = SafeMalloc(strlen(text)+1);
        strcpy(newLine->Text,text);
        History = newLine;
}

char *GetHistoryLine(int n)
{
        HISTORYLINE *rvp = History;
        int i;

        for (i=0; i<n; i++) {
                rvp = rvp->Next;
        }
        if (rvp)
                return &rvp->Text[0];
        else
                return "";
}

/*------------------------------------------------------------------------
 Procedure:     SendLastLine ID:1
 Purpose:       Sends the data in the line containing the cursor to
                the interpreter. If this is NOT the last line, copy
                the line to the end of the text.
 Input:         The edit control window handle
 Output:        None explicit
 Errors:        None
------------------------------------------------------------------------*/
void SendLastLine(HWND hEdit)
{
        int curline = GetCurLineIndex(hEdit);
        char *p,linebuffer[2048];
        int n;
        int linescount = GetNumberOfLines(hEdit);

        *(unsigned short *)linebuffer = sizeof(linebuffer)-1;
        if (curline != linescount-1)
          n = SendMessage(hEdit,EM_GETLINE,curline,(LPARAM)linebuffer);
        else
          n = SendMessage(hEdit,EM_GETLINE,curline-1,(LPARAM)linebuffer);
        if (n >= 2 && linebuffer[0] == '#' && linebuffer[1] == ' ') {
          n -= 2;
          memmove(linebuffer, linebuffer+2, n);
        }
        linebuffer[n] = 0;
        // Record user input!
        AddToHistory(linebuffer);
        linebuffer[n] = '\n';
        linebuffer[n+1] = 0;
        WriteToPipe(linebuffer);
        if (curline != linescount-1) {
                // Copy the line sent to the end of the text
                p = strrchr(linebuffer,'\n');
                if (p) {
                        *p = 0;
                }
                busy = 1;
                AddLineToControl(linebuffer);
                busy = 0;
        }
}
/*------------------------------------------------------------------------
 Procedure:     SetLastPrompt ID:1
 Purpose:       Record the position of the last prompt ("# ") sent by
                the interpreter. This isn't really used yet.
 Input:
 Output:
 Errors:
------------------------------------------------------------------------*/
void SetLastPrompt(HWND hEdit)
{
        DWORD startpos,endpos;
        SendMessage(hEdit,EM_GETSEL,(WPARAM)&startpos,(LPARAM)&endpos);
        LastPromptPosition.line = SendMessage(hEdit,EM_LINEFROMCHAR,(WPARAM)-1,0);
        LastPromptPosition.col = startpos;
}

/*------------------------------------------------------------------------
 Procedure:     MdiChildWndProc ID:1
 Purpose:       The edit control is enclosed in a normal MDI window.
                This is the window procedure for that window. When it
                receives the WM_CREATE message, it will create the
                edit control.
 Input:
 Output:
 Errors:
------------------------------------------------------------------------*/
static LRESULT CALLBACK MdiChildWndProc(HWND hwnd,UINT msg,WPARAM wparam,LPARAM lparam)
{
        HWND hwndChild;
        RECT rc;
        HDC hDC;

        switch(msg) {
        case WM_CREATE:
          GetClientRect(hwnd,&rc);
          hwndChild= CreateWindow("EDIT",
                                  NULL,
                                  WS_CHILD | WS_VISIBLE |
                                  ES_MULTILINE |
                                  WS_VSCROLL | WS_HSCROLL |
                                  ES_AUTOHSCROLL | ES_AUTOVSCROLL,
                                  0,
                                  0,
                                  (rc.right-rc.left),
                                  (rc.bottom-rc.top),
                                  hwnd,
                                  (HMENU) EditControls++,
                                  hInst,
                                  NULL);
          SetWindowLong(hwnd, DWL_USER, (DWORD) hwndChild);
          SendMessage(hwndChild, WM_SETFONT, (WPARAM) ProgramParams.hFont, 0L);
          SendMessage(hwndChild,EM_LIMITTEXT,0xffffffff,0);
          SubClassEditField(hwndChild);
          break;
          // Resize the edit control
        case WM_SIZE:
          hwndChild = (HWND) GetWindowLong(hwnd, DWL_USER);
          MoveWindow(hwndChild, 0, 0, LOWORD(lparam), HIWORD(lparam), TRUE);
          break;
          // Always set the focus to the edit control.
        case WM_SETFOCUS:
          hwndChild = (HWND) GetWindowLong(hwnd, DWL_USER);
          SetFocus(hwndChild);
          break;
          // Repainting of the edit control about to happen.
          // Set the text color and the background color
        case WM_CTLCOLOREDIT:
          hDC = (HDC)wparam;
          SetTextColor(hDC,ProgramParams.TextColor);
          SetBkColor(hDC,BackColor);
          return (LRESULT)BackgroundBrush;
          // Take care of erasing the background color to avoid flicker
        case WM_ERASEBKGND:
          GetWindowRect(hwnd,&rc);
          hDC = (HDC)wparam;
          FillRect(hDC,&rc,BackgroundBrush);
          return 1;
          // A carriage return has been pressed. Send the data to the interpreted.
          // This message is posted by the subclassed edit field.
        case WM_COMMAND:
          if (LOWORD(wparam) >= IDEDITCONTROL && LOWORD(wparam) < IDEDITCONTROL+5) {
            switch (HIWORD(wparam)) {
            case EN_ERRSPACE:
            case EN_MAXTEXT:
              ResetText();
              break;
            }
          }
          break;
        case WM_NEWLINE:
          if (busy)
            break;
          hwndChild = (HWND) GetWindowLong(hwnd, DWL_USER);
          SendLastLine(hwndChild);
          break;
          // The timer will call us 4 times a second. Look if the interpreter
          // has written something in its end of the pipe.
        case WM_TIMERTICK:
          hwndChild = (HWND) GetWindowLong(hwnd, DWL_USER);
          if (ReadToLineBuffer()) {
            char *p;
                                // Ok we read something. Display it.
            AddLineBuffer();
            p = strrchr(lineBuffer,'\r');
            if (p && !strcmp(p,"\r\n# ")) {
              if (p[4] == 0) {
                SetLastPrompt(hwndChild);
              }
            }

          }
          break;

        }
        return DefMDIChildProc(hwnd, msg, wparam, lparam);
}


/*------------------------------------------------------------------------
 Procedure:     MainWndProc ID:1
 Purpose:       Window procedure for the frame window, that contains
                the menu. The messages handled are:
                WM_CREATE: Creates the mdi child window
                WM_SIZE: resizes the status bar and the mdi child
                window
                WM_COMMAND: Sends the command to the dispatcher
                WM_CLOSE: If the user confirms, it exists the program
                WM_QUITOCAML: Stops the program unconditionally.
 Input:         Standard windows callback
 Output:
 Errors:
------------------------------------------------------------------------*/
static LRESULT CALLBACK MainWndProc(HWND hwnd,UINT msg,WPARAM wParam,LPARAM lParam)
{
        switch (msg) {
                // Create the MDI client invisible window
        case WM_CREATE:
                hwndMDIClient = CreateMdiClient(hwnd);
        TimerId = SetTimer((HWND) 0, 0, 100, (TIMERPROC) TimerProc);
                break;
        // Move the child windows
        case WM_SIZE:
                SendMessage(hWndStatusbar,msg,wParam,lParam);
                InitializeStatusBar(hWndStatusbar,1);
                // Position the MDI client window between the tool and status bars
                if (wParam != SIZE_MINIMIZED) {
                        RECT rc, rcClient;

                        GetClientRect(hwnd, &rcClient);
                        GetWindowRect(hWndStatusbar, &rc);
                        ScreenToClient(hwnd, (LPPOINT)&rc.left);
                        rcClient.bottom = rc.top;
                        MoveWindow(hwndMDIClient,rcClient.left,rcClient.top,rcClient.right-rcClient.left, rcClient.bottom-rcClient.top, TRUE);
                }

                return 0;
                // Dispatch the menu commands
        case WM_COMMAND:
                HandleCommand(hwnd, wParam,lParam);
                return 0;
                // If user confirms close
        case WM_CLOSE:
                if (!AskYesOrNo("Quit Ocaml?"))
                        return 0;
                break;
                // End application
        case WM_DESTROY:
                PostQuitMessage(0);
                break;
                // The interpreter has exited. Force close of the application
        case WM_QUITOCAML:
                DestroyWindow(hwnd);
                return 0;
        case WM_USER+1000:
                // TestGraphics();
                break;
        default:
                return DefFrameProc(hwnd,hwndMDIClient,msg,wParam,lParam);
        }
        return DefFrameProc(hwnd,hwndMDIClient,msg,wParam,lParam);
}

/*------------------------------------------------------------------------
 Procedure:     CreationCourier ID:1
 Purpose:       Creates the courier font
 Input:
 Output:
 Errors:
------------------------------------------------------------------------*/
static HFONT CreationCourier(int flag)
{
   LOGFONT CurrentFont;
   memset(&CurrentFont, 0, sizeof(LOGFONT));
   CurrentFont.lfCharSet = ANSI_CHARSET;
   CurrentFont.lfWeight = FW_NORMAL;
   if (flag)
      CurrentFont.lfHeight = 18;
   else
      CurrentFont.lfHeight = 15;
   CurrentFont.lfPitchAndFamily = (BYTE) (FIXED_PITCH | FF_MODERN);
   strcpy(CurrentFont.lfFaceName, "Courier");  /* Courier */
   return (CreateFontIndirect(&CurrentFont));
}

/*------------------------------------------------------------------------
 Procedure:     ReadToLineBuffer ID:1
 Purpose:       Reads into the line buffer the characters written by
                the interpreter
 Input:         None
 Output:        The number of characters read
 Errors:        None
------------------------------------------------------------------------*/
int ReadToLineBuffer(void)
{
        memset(lineBuffer,0,sizeof(lineBuffer));
        return ReadFromPipe(lineBuffer,sizeof(lineBuffer));
}

/*------------------------------------------------------------------------
 Procedure:     AddLineBuffer ID:1
 Purpose:       Sends the contents of the line buffer to the edit
                control
 Input:         None
 Output:
 Errors:
------------------------------------------------------------------------*/
int AddLineBuffer(void)
{
        HWND hEditCtrl;

        hEditCtrl = (HWND)GetWindowLong(hwndSession,DWL_USER);
        return SendMessage(hEditCtrl,EM_REPLACESEL,0,(LPARAM)lineBuffer);

}

/*------------------------------------------------------------------------
 Procedure:     Setup ID:1
 Purpose:       Handles GUI initialization (Fonts, brushes, colors,
                etc)
 Input:
 Output:
 Errors:
------------------------------------------------------------------------*/
static int Setup(HANDLE *phAccelTable)
{
        if (!InitApplication())
                return 0;
        ProgramParams.hFont = CreationCourier(1);
        ProgramParams.TextColor = RGB(0,0,0);
        GetObject(ProgramParams.hFont,sizeof(LOGFONT),&CurrentFont);
        BackgroundBrush = CreateSolidBrush(BackColor);
        *phAccelTable = LoadAccelerators(hInst,MAKEINTRESOURCE(IDACCEL));
        return 1;
}


/*------------------------------------------------------------------------
 Procedure:     WinMain ID:1
 Purpose:       Entry point for windows programs.
 Input:
 Output:
 Errors:
------------------------------------------------------------------------*/
int WINAPI WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance, LPSTR lpCmdLine, INT nCmdShow)
{
        MSG msg;
        HANDLE hAccelTable;
        char consoleTitle[512];
        HWND hwndConsole;

        // Setup the hInst global
        hInst = hInstance;
        // Do the setup
        if (!Setup(&hAccelTable))
                return 0;
        // Need to set up a console so that we can send ctrl-break signal
        // to inferior Caml
        AllocConsole();
        GetConsoleTitle(consoleTitle,sizeof(consoleTitle));
        hwndConsole = FindWindow(NULL,consoleTitle);
        ShowWindow(hwndConsole,SW_HIDE);
        // Create main window and exit if this fails
        if ((hwndMain = CreateinriaWndClassWnd()) == (HWND)0)
                return 0;
        // Create the status bar
        CreateSBar(hwndMain,"Ready",2);
        // Show the window
        ShowWindow(hwndMain,SW_SHOW);
        // Create the session window
        hwndSession = MDICmdFileNew("Session transcript",0);
        // Get the path to ocaml.exe
        GetOcamlPath();
        // Start the interpreter
        StartOcaml();
        // Show the session window
        ShowWindow(hwndSession, SW_SHOW);
        // Maximize it
    SendMessage(hwndMDIClient, WM_MDIMAXIMIZE, (WPARAM) hwndSession, 0);

        PostMessage(hwndMain,WM_USER+1000,0,0);
        while (GetMessage(&msg,NULL,0,0)) {
                if (!TranslateMDISysAccel(hwndMDIClient, &msg))
                        if (!TranslateAccelerator(msg.hwnd, hAccelTable, &msg)) {
                                TranslateMessage(&msg);  // Translates virtual key codes
                                DispatchMessage(&msg);   // Dispatches message to window
                        }
        }
        WriteToPipe("#quit;;\r\n\032");
    KillTimer((HWND) 0, TimerId);
        return msg.wParam;
}
