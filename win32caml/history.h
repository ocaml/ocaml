/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Jacob Navia, after Xavier Leroy                          */
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

#ifndef _HISTORY_H_
#define _HISTORY_H_

#include "editbuffer.h"

// Simple linked list for holding the history lines
typedef struct tagStatementHistory {
        struct tagStatementHistory	*Next;
		struct tagStatementHistory	*Prev;
        EditBuffer					*Statement;
} StatementHistory;

void	AddToHistory	(EditBuffer *edBuf);
char	*GetHistoryLine	(int n);
static BOOL CALLBACK HistoryDlgProc(HWND hDlg, UINT message, WPARAM wParam, LPARAM lParam);

#endif
