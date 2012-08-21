/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*  Developed by Jacob Navia.                                          */
/*                                                                     */
/*  Copyright 2001 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/***********************************************************************/
/* Changes made by Chris Watford to enhance the source editor          */
/* Began 14 Sept 2003 - watford@uiuc.edu                               */
/***********************************************************************/

/* $Id$ */

/*@@ Wedit generated application. Written Sat Jun 02 18:22:38 2001
@@header: D:\lcc\inria\inriares.h
@@resources: D:\lcc\inria\inria.rc
Do not edit outside the indicated areas */
/*<---------------------------------------------------------------------->*/

#include <stdio.h>
#include <windows.h>
#include <windowsx.h>
#include <commctrl.h>
#include <string.h>
#include <direct.h>
#include <Richedit.h>
#include "inriares.h"
#include "inria.h"

#define VK_BACKSPACE    0x108

/*<---------------------------------------------------------------------->*/
int EditControls = IDEDITCONTROL;
static WNDPROC lpEProc;
static char lineBuffer[1024*32];
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
StatementHistory *History = NULL;
StatementHistory *HistoryTail = NULL;
StatementHistory *historyEntry = NULL;
EditBuffer *CurrentEditBuffer = NULL; // current edit buffer

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
    return CreateWindow("inriaWndClass","OCamlWinPlus v1.9RC4",
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
    HWND hEdit = (HWND)GetWindowLongPtr(hwndSession,DWLP_USER);
    int linesCount = SendMessage(hEdit,EM_GETLINECOUNT,0,0);
    int lineindex = SendMessage(hEdit,EM_LINEINDEX,linesCount-1,0);
    int lastLineLength = SendMessage(hEdit,EM_LINELENGTH,linesCount-1,0);

    lineindex += lastLineLength;
    SendMessage(hEdit,EM_SETSEL,lineindex,lineindex);
}

/*------------------------------------------------------------------------
Procedure:     GotoPrompt ID:1
Author:        Chris Watford watford@uiuc.edu
Purpose:       Puts the cursor on the prompt line right after the '# '
Input:
Output:
Errors:
------------------------------------------------------------------------*/
void GotoPrompt(void)
{
    HWND hEdit = (HWND)GetWindowLongPtr(hwndSession,DWLP_USER);
    int lineindex = SendMessage(hEdit,EM_LINEINDEX,LastPromptPosition.line,0)+2;
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

/*------------------------------------------------------------------------
Procedure:     GetLastLine ID:1
Purpose:       Gets the data in the line containing the cursor to
               the interpreter.
Input:         The edit control window handle
Output:        None explicit
Errors:        None
------------------------------------------------------------------------*/
char* GetLastLine(HWND hEdit)
{
    int curline = GetCurLineIndex(hEdit);
    char *linebuffer = (char*)SafeMalloc(2048*sizeof(char));
    int n;
    int linescount = GetNumberOfLines(hEdit);

    *(unsigned short *)linebuffer = 2047;
    n = SendMessage(hEdit,EM_GETLINE,curline,(LPARAM)linebuffer);

    if (n >= 2 && linebuffer[0] == '#' && linebuffer[1] == ' ') {
        n -= 2;
        memmove(linebuffer, linebuffer+2, n);
    }

    linebuffer[n] = '\0';

    return linebuffer;
}

void DoHelp(HWND hwnd)
{
    char word[256];
    GetWordUnderCursor(hwnd,word,sizeof(word));
    MessageBox(NULL,word,"Aide pour:",MB_OK);
}

/*------------------------------------------------------------------------
Procedure:     RewriteCurrentEditBuffer ID:1
Purpose:       Rewrites what is at the prompt with the current contents of
               the edit buffer
Input:         None
Output:        None explicit
Errors:        None
------------------------------------------------------------------------*/
void RewriteCurrentEditBuffer(void)
{
    // get the editbox's handle
    HWND hEdit = (HWND)GetWindowLongPtr(hwndSession,DWLP_USER);

    // calculate what to highlight
    int linesCount = SendMessage(hEdit,EM_GETLINECOUNT,0,0);
    int lineindex = SendMessage(hEdit,EM_LINEINDEX,LastPromptPosition.line,0) + 2;
    int lastLine = SendMessage(hEdit,EM_LINEINDEX,linesCount-1,0) + SendMessage(hEdit,EM_LINELENGTH,linesCount-1,0) + 100;

    // delete the current text
    SendMessage(hEdit, EM_SETSEL, (WPARAM)lineindex, (LPARAM)lastLine);
    SendMessage(hEdit, EM_REPLACESEL, (WPARAM)TRUE, (LPARAM)"");

    {
        // loop through each line in the edit buffer and add it to the control
        LineList* line = CurrentEditBuffer->Lines;
        for(; line != NULL; line = line->Next)
        {
            // if there is a line before me, add a newline
            if(line->Prev != NULL)
                SendMessage(hEdit, EM_REPLACESEL, (WPARAM)TRUE, (LPARAM)"\r\n");

            // add the line
            SendMessage(hEdit, EM_REPLACESEL, (WPARAM)TRUE, (LPARAM)line->Text);
        }
    }
}

/*------------------------------------------------------------------------
Procedure:     RefreshCurrentEditBuffer ID:1
Purpose:       Rewrites what is in the CurrentEditBuffer with what is
               actually there
Input:         None
Output:        None explicit
Errors:        None
------------------------------------------------------------------------*/
void RefreshCurrentEditBuffer(void)
{
    // get the editbox's handle
    HWND hEdit = (HWND)GetWindowLongPtr(hwndSession,DWLP_USER);

    // get the last line index
    int linesCount = SendMessage(hEdit,EM_GETLINECOUNT,0,0) - 1;
    int i = 0, n = 0;

    // where to hold the line we grab
    char *linebuffer = (char*)SafeMalloc(2048*sizeof(char));
    *(unsigned short *)linebuffer = 2047;

    editbuffer_destroy(CurrentEditBuffer);
    CurrentEditBuffer = editbuffer_new();

    // loop through each line updating or adding it to the current edit buffer
    for( ; (i + LastPromptPosition.line) <= linesCount; i++)
    {
        n = SendMessage(hEdit, EM_GETLINE, (i + LastPromptPosition.line), (LPARAM)linebuffer);

        if ((n >= 2) && (linebuffer[0] == '#') && (linebuffer[1] == ' ')) {
            n -= 2;
            memmove(linebuffer, linebuffer+2, n);
        }

        linebuffer[n] = '\0';

        {   // remove line breaks and feeds
            char* ln = linebuffer;

            while((*ln) != 0)
            {
                switch((*ln))
                {
                    case '\r':
                    case '\n':
                        (*ln) = ' ';
                }

                ln++;
            }
        }

        editbuffer_addline(CurrentEditBuffer, linebuffer);
    }
}

/*------------------------------------------------------------------------
Procedure:     NextHistoryEntry ID:1
Purpose:       Scrolls to the next history entry
Input:         None
Output:        None explicit
Errors:        None
--------------------------------------------------------------------------
Edit History:
    17 Sept 2003 - Chris Watford watford@uiuc.edu
        - Added this as a helper function
    18 Sept 2003 - Chris Watford watford@uiuc.edu
        - Corrected doubly linked list problems
------------------------------------------------------------------------*/
void NextHistoryEntry(void)
{
    // out of bounds, put it back into bounds
    if(historyEntry == NULL && History == NULL)
    {
        return;
    } else if (historyEntry == NULL && History != NULL) {
        historyEntry = History;
    } else {
        if(historyEntry->Next == NULL)
            return;

        historyEntry = historyEntry->Next;
    }

    // if its valid
    if(historyEntry != NULL)
    {
        // copy the history entry to a new buffer
        EditBuffer* newBuf = editbuffer_copy(historyEntry->Statement);

        // destroy the old buffer
        editbuffer_destroy(CurrentEditBuffer);

        // setup the current one to the copy
        CurrentEditBuffer = newBuf;

        // rewrite the old one and go to the prompt
        RewriteCurrentEditBuffer();
        GotoPrompt();
    }
}

/*------------------------------------------------------------------------
Procedure:     PrevHistoryEntry ID:1
Purpose:       Scrolls to the previous history entry
Input:         None
Output:        None explicit
Errors:        None
--------------------------------------------------------------------------
Edit History:
    17 Sept 2003 - Chris Watford watford@uiuc.edu
        - Added this as a helper function
    18 Sept 2003 - Chris Watford watford@uiuc.edu
        - Corrected doubly linked list problems
------------------------------------------------------------------------*/
void PrevHistoryEntry(void)
{
    // out of bounds, put it back into bounds
    if(historyEntry == NULL || History == NULL)
    {
        return;
    } else {
        if(historyEntry->Prev == NULL)
            return;

        historyEntry = historyEntry->Prev;
    }

    // if its valid
    if(historyEntry != NULL)
    {
        // copy the history entry to a new buffer
        EditBuffer* newBuf = editbuffer_copy(historyEntry->Statement);

        // destroy the old buffer
        editbuffer_destroy(CurrentEditBuffer);

        // setup the current one to the copy
        CurrentEditBuffer = newBuf;

        // rewrite the old one and go to the prompt
        RewriteCurrentEditBuffer();
        GotoPrompt();
    }
}

/*------------------------------------------------------------------------
Procedure:     SubClassEdit ID:1
Purpose:       Handles messages to the editbox
Input:
Output:
Errors:
--------------------------------------------------------------------------
Edit History:
    14 Sept 2003 - Chris Watford watford@uiuc.edu
        - Setup handler for up and down arrows
    15 Sept 2003 - Chris Watford watford@uiuc.edu
        - Setup framework for history on up arrow
        - Saves lines you move off of in the edit buffer
    16 Sept 2003 - Chris Watford watford@uiuc.edu
        - Proper handling of newline message finished
        - Fixed ENTER on middle of interior line, moves cursor to the end
          and sends the line
        - Setup the copying and destroying of the old buffer
        - Included buffer rewrite
    17 Sept 2003 - Chris Watford watford@uiuc.edu
        - Added C-p/C-n support
        - Changed UpArrow to C-UpArrow so as to not confuse users
    18 Sept 2003 - Chris Watford watford@uiuc.edu
        - Added Left and Right arrow line saving
        - Added backspace and delete line saving and removing
        - Fixed history scrolling
    21 Sept 2003 - Chris Watford watford@uiuc.edu
        - Fixed pasting errors associated with lines being out of bounds
          for the buffer
        - Added error handling, possibly able to handle it diff down the
          line
        - Removed C-Up/C-Dn for history scrolling, buggy at best on my
          machine
------------------------------------------------------------------------*/
static LRESULT CALLBACK SubClassEdit(HWND hwnd, UINT msg, WPARAM mp1, LPARAM mp2)
{
    LRESULT r;
    int postit=0,nl;

    if (msg == WM_CHAR && mp1 == '\r') {
        if (!busy) {
            r =  GetCurLineIndex(hwnd);
            nl = GetNumberOfLines(hwnd);

            // if we're not the last line
            if (r != nl-1)
            {
                // update or add us, we might not have any lines in the edit buffer
                editbuffer_updateoraddline(CurrentEditBuffer, r-LastPromptPosition.line, GetLastLine(hwnd));

                // scroll to the end, add CrLf then post the newline message
                GotoEOF();
                AddStringToControl("\r\n");
                PostMessage(GetParent(hwnd),WM_NEWLINE,0,0);
                return 0;
            }

            CallWindowProc(lpEProc,hwnd,WM_KEYDOWN,VK_END,1);
            CallWindowProc(lpEProc,hwnd,WM_KEYUP,VK_END,1);

            postit = 1;
        }

    }
    else if (msg == WM_CHAR && mp1 == (char)0x08) {
        int lineindex = SendMessage(hwnd, EM_LINEINDEX, LastPromptPosition.line, 0) + 2;
        int curline = SendMessage(hwnd,EM_LINEFROMCHAR,(WPARAM)-1,0);
        int nextline = 0;
        int curpoint = 0;

        SendMessage(hwnd, EM_GETSEL, (WPARAM)&curpoint, (LPARAM)NULL);
        nextline = SendMessage(hwnd,EM_LINEFROMCHAR,(WPARAM)(curpoint - 1),0);

        if(curpoint <= lineindex)
        {
            return 0;
        } else if(nextline != curline) {
            // delete the line we're on

            // grab the index
            curline -= LastPromptPosition.line;

            // kill it
            editbuffer_removeline(CurrentEditBuffer, curline);
        }
    }
    else if (msg == WM_KEYDOWN && mp1 == VK_F1) {
        DoHelp(hwnd);
    }
    else if ((msg == WM_KEYDOWN || msg == WM_KEYUP) && mp1 == VK_UP) {
        int curline = GetCurLineIndex(hwnd);

        /*if((msg == WM_KEYDOWN) && (GetKeyState(VK_CONTROL) && 0x8000))
        {   // go forward once in history
            NextHistoryEntry();
            return 0;
        } else */
        if((curline > LastPromptPosition.line) && (curline <= (LastPromptPosition.line + CurrentEditBuffer->LineCount)))
        {
            // update current line
            if (msg == WM_KEYDOWN)
            {
                int lineidx = (curline - LastPromptPosition.line);

                CallWindowProc(lpEProc,hwnd,WM_KEYDOWN,VK_END,1);
                CallWindowProc(lpEProc,hwnd,WM_KEYUP,VK_END,1);

                // we may have to add this line, otherwise update it
                editbuffer_updateoraddline(CurrentEditBuffer, lineidx, GetLastLine(hwnd));
            }
        } else {
            return 0;
        }
    }
    else if ((msg == WM_KEYDOWN || msg == WM_KEYUP) && (mp1 == VK_LEFT)) {
        int lineindex = SendMessage(hwnd, EM_LINEINDEX, LastPromptPosition.line, 0) + 2;
        int curline = SendMessage(hwnd,EM_LINEFROMCHAR,(WPARAM)-1,0);
        int nextline = 0;
        int curpoint = 0;

        SendMessage(hwnd, EM_GETSEL, (WPARAM)&curpoint, (LPARAM)NULL);
        nextline = SendMessage(hwnd,EM_LINEFROMCHAR,(WPARAM)(curpoint - 1),0);

        if(curpoint <= lineindex)
        {   // no left arrow to the left of the prompt
            return 0;
        } else if(nextline != curline) {
            // update current line
            if (msg == WM_KEYDOWN)
            {
                int lineidx = (curline - LastPromptPosition.line);

                CallWindowProc(lpEProc,hwnd,WM_KEYDOWN,VK_END,1);
                CallWindowProc(lpEProc,hwnd,WM_KEYUP,VK_END,1);

                // we may have to add this line, otherwise update it
                editbuffer_updateoraddline(CurrentEditBuffer, lineidx, GetLastLine(hwnd));

                CallWindowProc(lpEProc,hwnd,WM_KEYDOWN,VK_HOME,1);
                CallWindowProc(lpEProc,hwnd,WM_KEYUP,VK_HOME,1);
            }
        }
    }
    else if ((msg == WM_KEYDOWN || msg == WM_KEYUP) && (mp1 == VK_DOWN)) {
        int curline = GetCurLineIndex(hwnd);

        /*if((msg == WM_KEYDOWN) && (GetKeyState(VK_CONTROL) && 0x8000))
        {   // go back once in history
            PrevHistoryEntry();
            return 0;
        } else*/
        if((curline >= LastPromptPosition.line) && (curline < (LastPromptPosition.line + CurrentEditBuffer->LineCount)))
        {
            // We don't post the newline, but instead update the current line
            if (msg == WM_KEYDOWN)
            {
                int lineidx = (curline - LastPromptPosition.line);

                CallWindowProc(lpEProc,hwnd,WM_KEYDOWN,VK_END,1);
                CallWindowProc(lpEProc,hwnd,WM_KEYUP,VK_END,1);

                editbuffer_updateline(CurrentEditBuffer, lineidx, GetLastLine(hwnd));
            }
        } else {
            return 0;
        }
    }
    else if ((msg == WM_KEYDOWN || msg == WM_KEYUP) && (mp1 == VK_RIGHT)) {
        int lineindex = SendMessage(hwnd, EM_LINEINDEX, LastPromptPosition.line, 0) + 1;
        int curline = SendMessage(hwnd,EM_LINEFROMCHAR,(WPARAM)-1,0);
        int nextline = 0;
        int curpoint = 0;

        SendMessage(hwnd, EM_GETSEL, (WPARAM)&curpoint, (LPARAM)NULL);
        nextline = SendMessage(hwnd,EM_LINEFROMCHAR,(WPARAM)(curpoint + 2),0);

        if(curpoint <= lineindex)
        {   // no movement behind the prompt
            return 0;
        } else if((nextline != curline) && (msg = WM_KEYDOWN)) {
            int lineidx = (curline - LastPromptPosition.line);

            CallWindowProc(lpEProc,hwnd,WM_KEYDOWN,VK_END,1);
            CallWindowProc(lpEProc,hwnd,WM_KEYUP,VK_END,1);

            editbuffer_updateline(CurrentEditBuffer, lineidx, GetLastLine(hwnd));
        }
    }
    else if ((msg == WM_KEYDOWN) && (mp1 == VK_PRIOR) && (GetKeyState(VK_CONTROL) && 0x8000)) {
        // C-p
        NextHistoryEntry();
        return 0;
    }
    else if ((msg == WM_KEYDOWN) && (mp1 == VK_NEXT) && (GetKeyState(VK_CONTROL) && 0x8000)) {
        // C-n
        PrevHistoryEntry();
        return 0;
    }
    else if ((msg == WM_KEYDOWN || msg == WM_KEYUP) && (mp1 == VK_DELETE)) {
        // see if we're the last char on the line, if so delete the next line
        // don't allow deleting left of the prompt
        int lineindex = SendMessage(hwnd, EM_LINEINDEX, LastPromptPosition.line, 0) + 2;
        int curline = SendMessage(hwnd,EM_LINEFROMCHAR,(WPARAM)-1,0);
        int nextline = 0;
        int curpoint = 0;

        SendMessage(hwnd, EM_GETSEL, (WPARAM)&curpoint, (LPARAM)NULL);
        nextline = SendMessage(hwnd,EM_LINEFROMCHAR,(WPARAM)(curpoint + 2),0);

        if(curpoint < lineindex)
        {   // no chomping behind the prompt
            return 0;
        } else if(nextline != curline) {
            // deleting
            // grab the next line index
            curline -= LastPromptPosition.line;

            // kill it
            editbuffer_removeline(CurrentEditBuffer, curline+1);
        }
    }
    else if (msg == WM_PASTE) {
        // if they paste text, allow it
        r = CallWindowProc(lpEProc, hwnd, msg, mp1, mp2);

        // update the current edit buffer
        RefreshCurrentEditBuffer();

        return r;
    }

    // handle errors
    switch(msg)
    {
        case WM_SYNTAXERROR:
        case WM_ILLEGALCHAR:
        case WM_UNBOUNDVAL:
            {   // currently I handle them all the same
                // get the start of the line
                int start = SendMessage(hwnd, EM_LINEINDEX, LastPromptPosition.line, 0) + 2;

                // get the statement that error'd
                NextHistoryEntry();

                // tell the history that the last line errored
                if(History != NULL)
                    if(History->Statement != NULL)
                        History->Statement->isCorrect = FALSE;

                // highlight the offending chars
                SendMessage(hwnd,EM_SETSEL,(WPARAM)(start + mp1), (LPARAM)(start + mp2));

                return 0;
            }
    }

    r = CallWindowProc(lpEProc, hwnd, msg, mp1, mp2);

    if (postit)
        PostMessage(GetParent(hwnd),WM_NEWLINE,0,0);

    return r;
}

static void SubClassEditField(HWND hwnd)
{
    if (lpEProc == NULL) {
        lpEProc = (WNDPROC) GetWindowLongPtr(hwnd, GWLP_WNDPROC);
    }
    SetWindowLongPtr(hwnd, GWLP_WNDPROC, (LONG_PTR) SubClassEdit);
}

/*------------------------------------------------------------------------
Procedure:     SendLastLine ID:1
Purpose:       Sends the data in the line containing the cursor to
the interpreter. If this is NOT the last line, copy
the line to the end of the text.
Input:         The edit control window handle
Output:        None explicit
Errors:        None

REMOVED!
------------------------------------------------------------------------*/
void SendLastLine(HWND hEdit)
{
/*  int curline = GetCurLineIndex(hEdit);
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
    }*/
}

/*------------------------------------------------------------------------
Procedure:     SendLastEditBuffer ID:1
Author:        Chris Watford watford@uiuc.edu
Purpose:       Sends an edit buffer to the pipe
Input:
Output:
Errors:
--------------------------------------------------------------------------
Edit History:
     7 Aug  2004 - Chris Watford christopher.watford@gmail.com
        - Fixed error where SendLastEditBuffer sent waaaay too many
        newlines which completely broke the underlying connection to the
        ocaml.exe pipe
    15 Sept 2003 - Chris Watford watford@uiuc.edu
        - Sends line to the pipe and adds newline to the end
------------------------------------------------------------------------*/
void SendLastEditBuffer(HWND hwndChild)
{
    char* line = editbuffer_getasbuffer(CurrentEditBuffer);
    int l = strlen(line) - 1;
    char* linebuffer = (char*)SafeMalloc(l+2);

    // save current edit buffer to history and create a new blank edit buffer
    CurrentEditBuffer->isCorrect = TRUE;
    AddToHistory(CurrentEditBuffer);
    CurrentEditBuffer = (EditBuffer*)SafeMalloc(sizeof(EditBuffer));
    CurrentEditBuffer->LineCount = 0;
    CurrentEditBuffer->Lines = NULL;

    // trim and add the newline to the end
    strncpy(linebuffer, line, l+1);
    while((linebuffer[l] == '\n' || linebuffer[l] == '\r') && (l >= 0))
    {
        linebuffer[l--] = '\0';
    }

    linebuffer[l+1] = '\n';
    linebuffer[l+2] = '\0';

    // save line to the pipe
    WriteToPipe(linebuffer);
}

/*------------------------------------------------------------------------
Procedure:     SendingFullCommand ID:1
Author:        Chris Watford watford@uiuc.edu
Purpose:       Returns if the command being sent
Input:         The edit control window handle
Output:        None explicit
Errors:        None
--------------------------------------------------------------------------
Edit History:
     7 Aug  2004 - Chris Watford christopher.watford@gmail.com
        - Fixed bug #2932 where many carraige returns were sent and it came
        back with a null pointer error due to a fault of not checking if
        the line returned was NULL
    13 Oct  2003 - Chris Watford watford@uiuc.edu
        - Solved the error when you have a malformed comment in the buffer
------------------------------------------------------------------------*/
BOOL SendingFullCommand(void)
{
    // if there is a ;; on the line, return true
    char *line = editbuffer_getasline(CurrentEditBuffer);
    char *firstComment, *firstSemiColonSemiColon, *firstQuote;

    if(line == NULL)
    {
        return FALSE;
    }

    firstComment = strstr(line, "(*");
    firstSemiColonSemiColon = strstr(line, ";;");
    firstQuote = strstr(line, "\"");

    // easy case :D
    if(firstSemiColonSemiColon == NULL)
    {
        free(line);
        return FALSE;
    }

    // if there are no comments
    if(firstComment == NULL)
    {
        // if there are no quotations used
        if(firstQuote == NULL)
        {
            BOOL r = (firstSemiColonSemiColon != NULL);
            free(line);
            return r;
        } else {
            // we need to first check if the ;; is before the \", since the \"
            // won't matter if its before the semicolonsemicolon
            if(firstQuote < firstSemiColonSemiColon)
            {
                // the quote is before the ;;, we need to make sure its terminated
                // also we have to check for escaped quotes, le sigh!
                char *c = firstQuote+1;
                BOOL in_quote = TRUE;

                // in-quote determiner loop
                while(c[0] != '\0')
                {
                    // are we a backslash?
                    if(c[0] == '\\')
                    {
                        // ignore the next character
                        c++;
                    }
                    else
                    {
                            // are we a quote?
                        if(c[0] == '"')
                        {
                            in_quote = !in_quote;
                        }
                    }

                    c++;
                }

                free(line);
                return !in_quote;
            } else {
                BOOL r = (firstSemiColonSemiColon != NULL);
                free(line);
                return r;
            }
        }
    } else {
        // we have to search through finding all comments

        // a neat little trick we can do is compare the point at which
        // the ;; is and where the first (* can be found, if the ;; is
        // before the (* ocaml.exe ignores the comment
        if((unsigned int)firstSemiColonSemiColon < (unsigned int)firstComment)
        {
            free(line);
            return TRUE;
        } else {
            // time to search and find if the endline is inside a comment or not
            // start at the first comment, and move forward keeping track of the
            // nesting level, if the nest level is 0, i.e. outside a comment
            // and we find the ;; return TRUE immediately, otherwise keep searching
            // if we end with a nest level >0 return FALSE

            char *c = firstComment+2; // firstComment[0] is the '(', firstComment[1] is the '*'
            int nestLevel = 1; // we have a (*

            // in-comment determiner loop
            while(c[0] != '\0')
            {
                // are we an endline
                if((c[0] == ';') && (c[1] == ';'))
                {
                    // if we are NOT in a comment, its a full line
                    if(nestLevel <= 0)
                    {
                        free(line);
                        return TRUE;
                    }
                }

                // are we in a comment?
                if((c[0] == '(') && (c[1] == '*'))
                {
                    nestLevel++;

                    // watch out we may go past the end
                    if(c[2] == '\0')
                    {
                        free(line);
                        return FALSE;
                    }

                    // c needs to advance past the *, cause (*) is NOT the start/finish of a comment
                    c++;
                }

                // adjust the nesting down a level
                if((c[0] == '*') && (c[1] == ')'))
                    nestLevel--;

                // next char
                c++;
            }

            // not a full line
            free(line);
            return FALSE;
        }
    }

    // weird case ;)
    free(line);
    return FALSE;
}

/*------------------------------------------------------------------------
Procedure:     AppendToEditBuffer ID:1
Author:        Chris Watford watford@uiuc.edu
Purpose:       Add a line to the edit buffer
Input:         Handle of the edit control
Output:
Errors:
------------------------------------------------------------------------*/
void AppendToEditBuffer(HWND hEdit)
{
    char *p = NULL, linebuffer[2048];
    int n = 0;
    int curline = GetCurLineIndex(hEdit);
    int linescount = GetNumberOfLines(hEdit);

    // they are passing the size of the buffer as
    // the first 'short' in the array...
    *(unsigned short *)linebuffer = sizeof(linebuffer)-1;

    if (curline > (linescount-1))
    {
        n = SendMessage(hEdit, EM_GETLINE, curline, (LPARAM)linebuffer);
    } else {
        n = SendMessage(hEdit, EM_GETLINE, --curline, (LPARAM)linebuffer);
    }

    // correct for the prompt line
    if (n >= 2 && linebuffer[0] == '#' && linebuffer[1] == ' ')
    {
        n -= 2;
        memmove(linebuffer, linebuffer+2, n);
    }

    linebuffer[n] = '\0';

    // linebuffer now has the line to add to our edit buffer
    editbuffer_updateoraddline(CurrentEditBuffer, (curline - LastPromptPosition.line), linebuffer);
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
--------------------------------------------------------------------------
Edit History:
    14 Sept 2003 - Chris Watford watford@uiuc.edu
        - Added edit buffer and statement buffer support to the WM_NEWLINE
          message.
    15 Sept 2003 - Chris Watford watford@uiuc.edu
        - Got it adding to the edit buffer
    16 Sept 2003 - Chris Watford watford@uiuc.edu
        - Proper handling of newline message finished
    21 Sept 2003 - Chris Watford watford@uiuc.edu
        - Added error detection on return from ocaml interp
    23 Sept 2003 - Chris Watford watford@uiuc.edu
        - Fixed prompt detection error as pointed out by Patrick Meredith
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
            SetWindowLongPtr(hwnd, DWLP_USER, (LONG_PTR) hwndChild);
            SendMessage(hwndChild, WM_SETFONT, (WPARAM) ProgramParams.hFont, 0L);
            SendMessage(hwndChild,EM_LIMITTEXT,0xffffffff,0);
            SubClassEditField(hwndChild);
            break;
            // Resize the edit control
        case WM_SIZE:
            hwndChild = (HWND) GetWindowLongPtr(hwnd, DWLP_USER);
            MoveWindow(hwndChild, 0, 0, LOWORD(lparam), HIWORD(lparam), TRUE);
            break;
            // Always set the focus to the edit control.
        case WM_SETFOCUS:
            hwndChild = (HWND) GetWindowLongPtr(hwnd, DWLP_USER);
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

            hwndChild = (HWND) GetWindowLongPtr(hwnd, DWLP_USER);

            // add what they wrote to the edit buffer
            AppendToEditBuffer(hwndChild);

        /** Modified by Chris Watford 14 Sept 2003, 15 Sept 2003, 16 Sept 2003 **/
            // test if this line has an end or if it needs to be in the Edit Buffer
            if(SendingFullCommand())
            {
                // send the edit buffer to the interpreter
                //SendLastLine(hwndChild);
                SendLastEditBuffer(hwndChild);
                historyEntry = NULL;
            } else {
                AddStringToControl("  ");
            }
        /** End Modifications **/

            break;
            // The timer will call us 4 times a second. Look if the interpreter
            // has written something in its end of the pipe.
        case WM_TIMERTICK:
        /** Modified by Chris Watford 21 Sept 2003 **/
            hwndChild = (HWND) GetWindowLongPtr(hwnd, DWLP_USER);

            if (ReadToLineBuffer())
            {
                int errMsg = 0;
                char *p, *l = lineBuffer;

                // Ok we read something. Display the trimmed version
                while(((*l) == ' ') || ((*l) == '\t') || ((*l) == '\n') || ((*l) == '\r') || ((*l) == '*'))
                    l++;

                SendMessage(hwndChild,EM_REPLACESEL,0,(LPARAM)l);

                // fix bug where it won't find prompt
                p = strrchr(l, '\r');
                if((l[0] == '#') || (p != NULL))
                {
                    if(p != NULL)
                    {
                        if(!strcmp(p, "\r\n# "))
                        {
                            SetLastPrompt(hwndChild);
                        }
                    // solve the bug Patrick found
                    } else if((l[0] == '#') && (l[1] == ' ')) {
                        SetLastPrompt(hwndChild);
                    }
                }

                // detect syntax errors
                if(strstr(lineBuffer, "Syntax error"))
                {
                    errMsg = WM_SYNTAXERROR;
                } else if(strstr(lineBuffer, "Illegal character")) {
                    errMsg = WM_ILLEGALCHAR;
                } else if(strstr(lineBuffer, "Unbound value")) {
                    errMsg = WM_UNBOUNDVAL;
                }

                // error! error! alert alert!
                if(errMsg > 0)
                {
                    int len = strlen(lineBuffer);
                    char* err = (char*)SafeMalloc(len+1);
                    char *m = err, *n1 = NULL, *n2 = NULL, *nt = NULL;

                    // make a copy of the message
                    strncpy(err, lineBuffer, len);
                    err[len] = '\0';

                    // find it
                    m = strstr(err, "Characters ");
                    if(m == NULL)
                        break;

                    // got the start char
                    n1 = m + strlen("Characters ");

                    // start looking for the end char
                    nt = strstr(n1, "-");
                    if(nt == NULL)
                        break;

                    // makes n1 a valid string
                    nt[0] = '\0';

                    // end char is right after this
                    n2 = nt + 1;

                    // find the end of n2
                    nt = strstr(n2, ":");
                    if(nt == NULL)
                        break;

                    // makes n2 a valid string
                    nt[0] = '\0';

                    SendMessage(hwndChild, errMsg, (WPARAM)atoi(n1), (LPARAM)atoi(n2));
                }
            }
        /** End Modifications **/

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
            if (!AskYesOrNo("Quit OCamlWinPlus?"))
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

    hEditCtrl = (HWND)GetWindowLongPtr(hwndSession,DWLP_USER);
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

    CurrentEditBuffer = (EditBuffer*)SafeMalloc(sizeof(EditBuffer));
    CurrentEditBuffer->LineCount = 0;
    CurrentEditBuffer->Lines = NULL;

    //setup the history index pointer
    historyEntry = NULL;

    // Setup the hInst global
    hInst = hInstance;
    // Do the setup
    if (!Setup(&hAccelTable))
        return 0;
    // Need to set up a console so that we can send ctrl-break signal
    // to inferior OCaml
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
