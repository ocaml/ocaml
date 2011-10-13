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

#include <stdio.h>
#include <windows.h>
#include <Richedit.h>
#include "inria.h"
#include "inriares.h"
#include "history.h"

LOGFONT CurrentFont;
int CurrentFontFamily = (FIXED_PITCH | FF_MODERN);
int CurrentFontStyle;
char CurrentFontName[64] = "Courier";

/*------------------------------------------------------------------------
 Procedure:     OpenMlFile ID:1
 Purpose:       Opens a file, either a source file (*.ml) or an *.cmo
                file.
 Input:         A buffer where the name will be stored, and its
                length
 Output:        The user's choice will be stored in the buffer.
 Errors:        None
------------------------------------------------------------------------*/
int OpenMlFile(char *fname,int lenbuf)
{
        OPENFILENAME ofn;
        int r;
        char *p,defext[5],tmp[512];

        memset(&ofn,0,sizeof(OPENFILENAME));
        memset(tmp,0,sizeof(tmp));
        fname[0] = 0;
        strcpy(tmp,"ocaml sources|*.ml|bytecode object files|*.cmo|All files|*.*");
        p = tmp;
        while (*p) {
                if (*p == '|')
                        *p = 0;
                p++;
        }
        strcpy(defext,"ml");
        ofn.lStructSize = sizeof(OPENFILENAME);
        ofn.hwndOwner = hwndMain;
        ofn.lpstrFilter = tmp;
        ofn.nFilterIndex = 1;
        ofn.hInstance = hInst;
        ofn.lpstrFile = fname;
        ofn.lpstrTitle = "Open file";
        ofn.lpstrInitialDir = LibDir;
        ofn.nMaxFile = lenbuf;
        ofn.Flags = OFN_PATHMUSTEXIST | OFN_NOCHANGEDIR | OFN_LONGNAMES |
                OFN_HIDEREADONLY |OFN_EXPLORER;
        r = GetOpenFileName(&ofn);
        if (r) {
          /* Replace backslashes by forward slashes in file name */
          for (p = fname; *p != 0; p++)
            if (*p == '\\') *p = '/';
        }
        return r;
}

/*------------------------------------------------------------------------
 Procedure:     GetSaveName ID:1
 Purpose:       Get a name to save the current session (Save as menu
                item)
 Input:         A buffer where the name of the file will be stored,
                and its length
 Output:        The name of the file choosen by the user will be
                stored in the buffer
 Errors:        none
------------------------------------------------------------------------*/
int GetSaveName(char *fname,int lenbuf)
{
        OPENFILENAME ofn;
        int r;
        char *p,defext[5],tmp[512];

        memset(&ofn,0,sizeof(OPENFILENAME));
        memset(tmp,0,sizeof(tmp));
        fname[0] = 0;
        strcpy(tmp,"Text files|*.txt");
        p = tmp;
        while (*p) {
                if (*p == '|')
                        *p = 0;
                p++;
        }
        strcpy(defext,"txt");
        ofn.lStructSize = sizeof(OPENFILENAME);
        ofn.hwndOwner = hwndMain;
        ofn.lpstrFilter = tmp;
        ofn.nFilterIndex = 1;
        ofn.hInstance = hInst;
        ofn.lpstrFile = fname;
        ofn.lpstrTitle = "Save as";
        ofn.lpstrInitialDir = LibDir;
        ofn.nMaxFile = lenbuf;
        ofn.Flags =  OFN_NOCHANGEDIR | OFN_LONGNAMES |
                OFN_HIDEREADONLY |OFN_EXPLORER;
        r = GetSaveFileName(&ofn);
        if (r == 0)
                return 0;
        else return 1;
}

/*------------------------------------------------------------------------
 Procedure:     GetSaveMLName ID:1
 Purpose:       Get a name to save the current OCaml code to (Save as menu
                item)
 Input:         A buffer where the name of the file will be stored,
                and its length
 Output:        The name of the file choosen by the user will be
                stored in the buffer
 Errors:        none
------------------------------------------------------------------------*/
int GetSaveMLName(char *fname, int lenbuf)
{
        OPENFILENAME ofn;
        int r;
        char *p,defext[5],tmp[512];

        memset(&ofn,0,sizeof(OPENFILENAME));
        memset(tmp,0,sizeof(tmp));
        fname[0] = 0;
        strcpy(tmp,"OCaml Source Files|*.ml");
        p = tmp;
        while (*p) {
                if (*p == '|')
                        *p = 0;
                p++;
        }
        strcpy(defext,"ml");
        ofn.lStructSize = sizeof(OPENFILENAME);
        ofn.hwndOwner = hwndMain;
        ofn.lpstrFilter = tmp;
        ofn.nFilterIndex = 1;
        ofn.hInstance = hInst;
        ofn.lpstrFile = fname;
        ofn.lpstrTitle = "Save as";
        ofn.lpstrInitialDir = LibDir;
        ofn.nMaxFile = lenbuf;
        ofn.Flags =  OFN_NOCHANGEDIR | OFN_LONGNAMES |
                OFN_HIDEREADONLY |OFN_EXPLORER;
        r = GetSaveFileName(&ofn);
        if (r == 0)
                return 0;
        else return 1;
}

/*------------------------------------------------------------------------
 Procedure:     BrowseForFile ID:1
 Purpose:       Let's the user browse for a certain kind of file.
                Currently this is only used when browsing for
                ocaml.exe.
 Input:         The name of the file to browse for, and the path
                where the user's choice will be stored.
 Output:        1 if user choosed a path, zero otherwise
 Errors:        None
------------------------------------------------------------------------*/
int BrowseForFile(char *fname,char *path)
{
        OPENFILENAME ofn;
        char *p,tmp[512],browsefor[512];
        int r;

        memset(tmp,0,sizeof(tmp));
        strncpy(tmp,fname,sizeof(tmp)-1);
        p = tmp;
        while (*p) {
                if (*p == '|')
                        *p = 0;
                p++;
        }
        memset(&ofn,0,sizeof(OPENFILENAME));
        ofn.lpstrFilter = tmp;
        ofn.nFilterIndex = 1;
        ofn.lStructSize = sizeof(OPENFILENAME);
        ofn.hwndOwner = hwndMain;
        ofn.hInstance = hInst;
        ofn.lpstrFilter = tmp;
        ofn.lpstrFile = path;
        wsprintf(browsefor,"Open %s",fname);
        ofn.lpstrTitle = browsefor;
        ofn.lpstrInitialDir = "c:\\";
        ofn.nMaxFile = MAX_PATH;
        ofn.Flags = OFN_PATHMUSTEXIST | OFN_NOCHANGEDIR | OFN_LONGNAMES |
                OFN_HIDEREADONLY |OFN_EXPLORER;
        r = GetOpenFileName(&ofn);
        if (r == 0)
                return 0;
        else return 1;
}

/*------------------------------------------------------------------------
 Procedure:     CallChangeFont ID:1
 Purpose:       Calls the standard windows font change dialog. If the
                user validates a font, it will destroy the current
                font, and recreate a new font with the given
                parameters.
 Input:         The calling window handle
 Output:        Zero if the user cancelled, 1 otherwise.
 Errors:        None
------------------------------------------------------------------------*/
static int CallChangeFont(HWND hwnd)
{
        LOGFONT lf;
        CHOOSEFONT cf;
        int r;
        HWND hwndChild;

        memset(&cf, 0, sizeof(CHOOSEFONT));
        memcpy(&lf, &CurrentFont, sizeof(LOGFONT));
        cf.lStructSize = sizeof(CHOOSEFONT);
        cf.hwndOwner = hwnd;
        cf.lpLogFont = &lf;
        cf.Flags = CF_SCREENFONTS | CF_EFFECTS | CF_APPLY | CF_INITTOLOGFONTSTRUCT;
        cf.nFontType = SCREEN_FONTTYPE;
        r = ChooseFont(&cf);
        if (!r)
                return (0);
        DeleteObject(ProgramParams.hFont);
        memcpy(&CurrentFont, &lf, sizeof(LOGFONT));
        ProgramParams.hFont = CreateFontIndirect(&CurrentFont);
        strcpy(CurrentFontName, CurrentFont.lfFaceName);
        CurrentFontFamily = lf.lfPitchAndFamily;
        CurrentFontStyle = lf.lfWeight;
    hwndChild = (HWND) GetWindowLongPtr(hwndSession, DWLP_USER);
        SendMessage(hwndChild,WM_SETFONT,(WPARAM)ProgramParams.hFont,0);
        ForceRepaint();
        return (1);
}

/*------------------------------------------------------------------------
 Procedure:     CallDlgProc ID:1
 Purpose:       Calls a dialog box procedure
 Input:         The function to call, and the numerical ID of the
                resource where the dialog box is stored
 Output:        Returns the result of the dialog box.
 Errors:        None
------------------------------------------------------------------------*/
int CallDlgProc(BOOL (CALLBACK *fn)(HWND,UINT,WPARAM,LPARAM), int id)
{
   int result;

   result = DialogBoxParam(hInst, MAKEINTRESOURCE(id), GetActiveWindow(),
                        fn, 0);
   return result;
}


/*------------------------------------------------------------------------
 Procedure:     CallChangeColor ID:1
 Purpose:       Calls the standard color dialog of windows, starting
                with the given color reference. The result is the
                same as the input if the user cancels, or another
                color if the user validates another one.
 Input:         The starting color
 Output:        The color the user has choosen.
 Errors:        None
------------------------------------------------------------------------*/
static COLORREF CallChangeColor(COLORREF InitialColor)
{
        CHOOSECOLOR CC;
        COLORREF CustColors[16];
        int r, g, b, i;
        memset(&CC, 0, sizeof(CHOOSECOLOR));
        r = g = b = 0;
        for (i = 0; i < 16; i++) {
                CustColors[i] = RGB(r, g, b);
                if (r < 255)
                        r += 127;
                else if (g < 255)
                        g += 127;
                else if (b < 255)
                        g += 127;
        }
        CC.lStructSize = sizeof(CHOOSECOLOR);
        CC.hwndOwner = hwndMain;
        CC.hInstance = hInst;
        CC.rgbResult = InitialColor;
        CC.lpCustColors = CustColors;
        CC.Flags = CC_RGBINIT;
        if (!ChooseColor(&CC))
                return (InitialColor);
        return (CC.rgbResult);
}

/*------------------------------------------------------------------------
 Procedure:     CallPrintSetup ID:1
 Purpose:       Calls the printer setup dialog. Currently it is not
                connected to the rest of the software, since printing
                is not done yet
 Input:         None
 Output:        1 if OK, 0, user cancelled
 Errors:        None
------------------------------------------------------------------------*/
static int CallPrintSetup(void)
{
        PAGESETUPDLG sd;
        int r;

        memset(&sd,0,sizeof(sd));
        sd.lStructSize = sizeof(sd);
        sd.Flags = PSD_RETURNDEFAULT;
        r = PageSetupDlg(&sd);
        if (!r)
                return 0;
        sd.Flags = 0;
        r = PageSetupDlg(&sd);
        return r;
}


/*------------------------------------------------------------------------
 Procedure:     Undo ID:1
 Purpose:       Send an UNDO command to the edit field.
 Input:         The parent window of the control
 Output:        None
 Errors:        None
------------------------------------------------------------------------*/
void Undo(HWND hwnd)
{
        HWND hEdit;

        hEdit = (HWND)GetWindowLongPtr(hwnd,DWLP_USER);
        SendMessage(hEdit,EM_UNDO,0,0);
}

/*------------------------------------------------------------------------
 Procedure:     ForceRepaint ID:1
 Purpose:       Forces a complete redraw of the edit control of the
                current session.
 Input:         None
 Output:        None
 Errors:        None
------------------------------------------------------------------------*/
void ForceRepaint(void)
{
        HWND hwndEdit = (HWND)GetWindowLongPtr(hwndSession,DWLP_USER);
        InvalidateRect(hwndEdit,NULL,1);
}

/*------------------------------------------------------------------------
 Procedure:     Add_Char_To_Queue ID:1
 Purpose:       Puts a character onto the buffer
 Input:         The char to be added
 Output:        None
 Errors:
------------------------------------------------------------------------*/
static void Add_Char_To_Queue(int c)
{
        HWND hwndEdit = (HWND)GetWindowLongPtr(hwndSession,DWLP_USER);
        SendMessage(hwndEdit,WM_CHAR,c,1);
}

/*------------------------------------------------------------------------
 Procedure:     AddLineToControl ID:1
 Purpose:       It will ad the given text at the end of the edit
                control, then it will send a return character to it.
                This simulates user input. The history will not be
                modified by this procedure.
 Input:         The text to be added
 Output:        None
 Errors:        If the line is empty, nothing will be done
------------------------------------------------------------------------*/
void AddLineToControl(char *buf)
{
        HWND hEditCtrl;

        if (*buf == 0)
                return;

        hEditCtrl = (HWND)GetWindowLongPtr(hwndSession,DWLP_USER);

        GotoEOF();

        SendMessage(hEditCtrl,EM_REPLACESEL,0,(LPARAM)buf);
        SendMessage(hEditCtrl,WM_CHAR,'\r',0);
}

/*------------------------------------------------------------------------
 Procedure:     AddStringToControl ID:1
 Author:        Chris Watford watford@uiuc.edu
 Purpose:       It will ad the given text at the end of the edit
                control. This simulates user input. The history will not
                be modified by this procedure.
 Input:         The text to be added
 Output:        None
 Errors:        If the line is empty, nothing will be done
--------------------------------------------------------------------------
Edit History:
    16 Sept 2003 - Chris Watford watford@uiuc.edu
        - Basically this is AddLineToControl, but without appending a
          newline
------------------------------------------------------------------------*/
void AddStringToControl(char* buf)
{
        HWND hEditCtrl;

        if(buf == NULL)
            return;

        if((*buf) == 0)
            return;

        hEditCtrl = (HWND)GetWindowLongPtr(hwndSession, DWLP_USER);
        GotoEOF();

        SendMessage(hEditCtrl ,EM_REPLACESEL, (WPARAM)FALSE, (LPARAM)buf);
}

/*------------------------------------------------------------------------
 Procedure:     AboutDlgProc ID:1
 Purpose:       Shows the "About" dialog box
 Input:
 Output:
 Errors:
------------------------------------------------------------------------*/
static BOOL CALLBACK AboutDlgProc(HWND hDlg, UINT message, WPARAM wParam, LPARAM lParam)
{
        if (message == WM_CLOSE)
                EndDialog(hDlg,1);
        return 0;
}

/*------------------------------------------------------------------------
 Procedure:     HistoryDlgProc ID:1
 Purpose:       Shows the history of the session. Only input lines
                are shown. A double click in a line will make this
                dialog box procedure return the index of the selected
                line (1 based). If the windows is closed (what is
                equivalent to cancel), the return value is zero.
 Input:         Normal windows callback
 Output:
 Errors:
--------------------------------------------------------------------------
Edit History:
    15 Sept 2003 - Chris Watford watford@uiuc.edu
        - Added support for my StatementHistory structure
        - Added the ability to export it as its exact entry, rather than
          just a 1 liner
------------------------------------------------------------------------*/
static BOOL CALLBACK HistoryDlgProc(HWND hDlg, UINT message, WPARAM wParam, LPARAM lParam)
{
        StatementHistory *histentry;
        int idx;
        RECT rc;

        switch (message) {
                case WM_INITDIALOG:
                        SendDlgItemMessage(hDlg,IDLIST,WM_SETFONT,(WPARAM)ProgramParams.hFont,0);
                        histentry = History; // get our statement history object
                        idx = 0;

                        // loop through each history entry adding it to the dialog
                        while (histentry != NULL) {
                                SendDlgItemMessage(hDlg,IDLIST,LB_INSERTSTRING,0,(LPARAM)editbuffer_getasline(histentry->Statement));
                                SendDlgItemMessage(hDlg,IDLIST,LB_SETITEMDATA,0,(LPARAM)idx);
                                histentry = histentry->Next;
                                idx++;
                        }

                        SendDlgItemMessage(hDlg,IDLIST,LB_SETCURSEL,(LPARAM)idx-1,0);
                        return 1;
                case WM_COMMAND:
                        switch(LOWORD(wParam)) {
                                case IDLIST:
                                        switch(HIWORD(wParam)) {
                                                case LBN_DBLCLK:
                                                        idx = SendDlgItemMessage(hDlg,IDLIST,LB_GETCURSEL,0,0);
                                                        if (idx == LB_ERR)
                                                                break;
                                                        idx = SendDlgItemMessage(hDlg,IDLIST,LB_GETITEMDATA,idx,0);
                                                        EndDialog(hDlg,idx+1);
                                                        return 1;
                                        }
                                        break;
                        }
                        break;
                case WM_SIZE:
                        GetClientRect(hDlg,&rc);
                        MoveWindow(GetDlgItem(hDlg,IDLIST),0,0,rc.right,rc.bottom,1);
                        break;

                case WM_CLOSE:
                        EndDialog(hDlg,0);
                        break;
        }
        return 0;
}

/*------------------------------------------------------------------------
 Procedure:     SaveText ID:1
 Purpose:       Saves the contents of the session transcript. It will
                loop for each line and write it to the specified file
 Input:         The name of the file where the session will be saved
 Output:        The session is saved
 Errors:        If it can't open the file for writing it will show an
                error box
--------------------------------------------------------------------------
 Edit History:
    06 Oct  2003 - Chris Watford watford@uiuc.edu
        - Corrected wsprintf error
------------------------------------------------------------------------*/
static void SaveText(char *fname)
{
        int i,len;
        HWND hEdit = (HWND)GetWindowLongPtr(hwndSession,DWLP_USER);
        int linesCount = SendMessage(hEdit,EM_GETLINECOUNT,0,0);
        FILE *f;
        char *buf = SafeMalloc(8192);

        f = fopen(fname,"wb");
        if (f == NULL)
        {
            // corrected error using wsprintf
            wsprintf(buf, "Impossible to open %s for writing", fname);

            ShowDbgMsg(buf);
            return;
        }

        for (i = 0; i < linesCount; i++)
        {
                *(unsigned short *)buf = 8100;
                len = SendMessage(hEdit, EM_GETLINE, i, (LPARAM)buf);
                buf[len] = '\0';
                fprintf(f, "%s\r\n", buf+1);
                //fwrite(buf,1,len+2,f);
        }

        fclose(f);
        free(buf);
}

/*------------------------------------------------------------------------
 Procedure:     SaveML ID:1
 Author:        Chris Watford watford@uiuc.edu
 Purpose:       Saves the ML source to a file, commenting out functions
                that contained errors
 Input:         The name of the file where the session will be saved
 Output:        The session is saved
 Errors:        If it can't open the file for writing it will show an
                error box
------------------------------------------------------------------------*/
static void SaveML(char *fname)
{
        FILE *f;
        char *buf = SafeMalloc(8192);

        f = fopen(fname, "wb");

        if(f == NULL)
        {
                wsprintf(buf, "Impossible to open %s for writing", fname);
                ShowDbgMsg(buf);
                return;
        }

        fprintf(f, "(* %s *)\r\n\r\n", fname);

        if(History != NULL)
        {
            StatementHistory *h = NULL;
            EditBuffer *stmt = NULL;

            // get to the end
            for(h = History; h->Next != NULL; h = h->Next);

            // go back :(
            // this is NOT the fastest method, BUT this is the easiest
            // on the subsystem
            for(; h != NULL; h = h->Prev)
            {
                stmt = h->Statement;

                if(stmt != NULL)
                {
                    // comment out incorrect lines
                    if(stmt->isCorrect)
                    {
                        char *buff = editbuffer_getasbuffer(stmt);
                        fprintf(f, "%s\r\n", buff);
                        free(buff);
                    } else {
                        char *buff = editbuffer_getasbuffer(stmt);
                        fprintf(f, "(* Syntax Error or Unbound Value\r\n%s\r\n *)\r\n", buff);
                        free(buff);
                    }
                }

                fprintf(f, "\r\n");
            }
        }

        fclose(f);
        free(buf);
}

/*------------------------------------------------------------------------
 Procedure:     Add_Clipboard_To_Queue ID:1
 Author:        Chris Watford watford@uiuc.edu
 Purpose:       Adds the clipboard text to the control
 Input:
 Output:
 Errors:
--------------------------------------------------------------------------
 Edit History:
    16 Sept 2003 - Chris Watford watford@uiuc.edu
        - Added method to update edit buffer with paste contents
------------------------------------------------------------------------*/
static void Add_Clipboard_To_Queue(void)
{
    if (IsClipboardFormatAvailable(CF_TEXT) && OpenClipboard(hwndMain))
    {
        HANDLE hClipData = GetClipboardData(CF_TEXT);

        if (hClipData != NULL)
        {
            char *str = GlobalLock(hClipData);

            if (str != NULL)
            {
                while ((*str) != 0)
                {
                    if (*str != '\r')
                        Add_Char_To_Queue(*str);

                    str++;
                }

                // added to fix odd errors
                RefreshCurrentEditBuffer();
            }

            GlobalUnlock(hClipData);
        }

        CloseClipboard();
    }
}

/*------------------------------------------------------------------------
 Procedure:     CopyToClipboard ID:1
 Purpose:       Copies text to the clipboard
 Input:         Window with the edit control
 Output:
 Errors:
------------------------------------------------------------------------*/
static void CopyToClipboard(HWND hwnd)
{
        HWND hwndEdit = (HWND)GetWindowLongPtr(hwndSession,DWLP_USER);
        SendMessage(hwndEdit,WM_COPY,0,0);
}

/*------------------------------------------------------------------------
 Procedure:     ResetText ID:1
 Purpose:       Resets the text? I'm not really sure
 Input:
 Output:        Always returns 0
 Errors:
------------------------------------------------------------------------*/
int ResetText(void)
{
        HWND hwndEdit = (HWND) GetWindowLongPtr(hwndSession,DWLP_USER);
        TEXTRANGE cr;
        int len = SendMessage(hwndEdit,WM_GETTEXTLENGTH,0,0);
        char *tmp = malloc(len+10),*p;

        memset(tmp,0,len+10);
        cr.chrg.cpMin = 0;
        cr.chrg.cpMax = -1;
        cr.lpstrText = tmp;
        SendMessage(hwndEdit,EM_GETTEXTRANGE,0,(LPARAM)&cr);
        p = tmp+len/2;
        while (*p && *p != '\r')
                p++;
        SendMessage(hwndEdit,EM_SETSEL,0,(LPARAM)-1);
        SendMessage(hwndEdit,EM_REPLACESEL,0,(LPARAM)p);
        InvalidateRect(hwndEdit,0,1);
        free(tmp);
        return 0;
}

/*------------------------------------------------------------------------
 Procedure:     HandleCommand ID:1
 Purpose:       Handles all menu commands.
 Input:
 Output:
 Errors:
--------------------------------------------------------------------------
 Edit History:
    06 Oct  2003 - Chris Watford watford@uiuc.edu
        - Removed entries that crashed OCaml
        - Removed useless entries
        - Added Save ML and Save Transcript
------------------------------------------------------------------------*/
void HandleCommand(HWND hwnd, WPARAM wParam,LPARAM lParam)
{
        char *fname;
        int r;

        switch(LOWORD(wParam)) {
                case IDM_OPEN:
                        fname = SafeMalloc(512);
                        if (OpenMlFile(fname,512)) {
                                char *buf = SafeMalloc(512);
                                char *p = strrchr(fname,'.');
                                if (p && !stricmp(p,".ml")) {
                                        wsprintf(buf, "#use \"%s\";;", fname);
                                        AddLineToControl(buf);
                                }
                                else if (p && !stricmp(p,".cmo")) {
                                        wsprintf(buf, "#load \"%s\";;", fname);
                                        AddLineToControl(buf);
                                }
                                free(buf);
                        }
                        free(fname);
                        break;
                case IDM_GC:
                        AddLineToControl("Gc.full_major();;");
                        break;
                case IDCTRLC:
                        InterruptOcaml();
                        break;
                case IDM_EDITPASTE:
                        Add_Clipboard_To_Queue();
                        break;
                case IDM_EDITCOPY:
                        CopyToClipboard(hwnd);
                        break;

                // updated to save a transcript
                case IDM_SAVEAS:
                        fname = SafeMalloc(512);
                        if (GetSaveName(fname,512)) {
                                SaveText(fname);
                        }
                        free(fname);
                        break;

                // updated to save an ML file
                case IDM_SAVE:
                        fname = SafeMalloc(512);
                        if (GetSaveMLName(fname,512))
                        {
                                SaveML(fname);
                        }
                        free(fname);
                        break;

                // updated to work with new history system
                case IDM_HISTORY:
                        r = CallDlgProc(HistoryDlgProc,IDD_HISTORY);

                        if (r)
                        {
                                AddLineToControl(GetHistoryLine(r-1));
                        }
                        break;

                case IDM_PRINTSU:
                        // Removed by Chris Watford
                        // seems to die
                        // CallPrintSetup();
                        break;

                case IDM_FONT:
                        CallChangeFont(hwndMain);
                        break;
                case IDM_COLORTEXT:
                        ProgramParams.TextColor = CallChangeColor(ProgramParams.TextColor);
                        ForceRepaint();
                        break;
                case IDM_BACKCOLOR:
                        BackColor = CallChangeColor(BackColor);
                        DeleteObject(BackgroundBrush);
                        BackgroundBrush = CreateSolidBrush(BackColor);
                        ForceRepaint();
                        break;
                case IDM_EDITUNDO:
                        Undo(hwnd);
                        break;

                /* Removed, really not very useful in this IDE
                case IDM_WINDOWTILE:
                        SendMessage(hwndMDIClient,WM_MDITILE,0,0);
                        break;
                case IDM_WINDOWCASCADE:
                        SendMessage(hwndMDIClient,WM_MDICASCADE,0,0);
                        break;
                case IDM_WINDOWICONS:
                        SendMessage(hwndMDIClient,WM_MDIICONARRANGE,0,0);
                        break;
                */

                case IDM_EXIT:
                        PostMessage(hwnd,WM_CLOSE,0,0);
                        break;
                case IDM_ABOUT:
                        CallDlgProc(AboutDlgProc,IDD_ABOUT);
                        break;
                default:
                        if (LOWORD(wParam) >= IDEDITCONTROL && LOWORD(wParam) < IDEDITCONTROL+5) {
                                switch (HIWORD(wParam)) {
                                        case EN_ERRSPACE:
                                                ResetText();
                                                break;
                                }
                        }
                        break;
        }
}
