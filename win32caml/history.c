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

#include "inria.h"
#include "history.h"

/*------------------------------------------------------------------------
Procedure:     AddToHistory ID:2
Author:		   Chris Watford watford@uiuc.edu
Purpose:       Adds an edit buffer to the history control
Input:		   Pointer to the edit buffer to add
Output:
Errors:
--------------------------------------------------------------------------
Edit History:
	15 Sept 2003 - Chris Watford watford@uiuc.edu
		- Complete rewrite
		- Got it to add the edit buffer to the history
	17 Sept 2003 - Chris Watford watford@uiuc.edu
		- Added doubly link list support
------------------------------------------------------------------------*/
void AddToHistory(EditBuffer *edBuf)
{
	StatementHistory *newLine;

	// sanity checks
	if(edBuf == NULL)
	{
		return;
	} else if (edBuf->LineCount == 0 || edBuf->Lines == NULL) {
		// fix any possible errors that may come from this
		edBuf->LineCount = 0;
		edBuf->Lines = NULL;
		return;
	}

	// setup newline and add as the front of the linked list
	newLine = SafeMalloc(sizeof(StatementHistory));
	newLine->Next = History;
	newLine->Prev = NULL;
	newLine->Statement = edBuf;

	// setup back linking
	if(History != NULL)
		History->Prev = newLine;

	// set the history up
	History = newLine;

	// search for the new history tail
	for(HistoryTail = (HistoryTail != NULL ? HistoryTail : History); HistoryTail->Next != NULL; HistoryTail = HistoryTail->Next);
}

/*------------------------------------------------------------------------
Procedure:     GetHistoryLine ID:2
Author:		   Chris Watford watford@uiuc.edu
Purpose:       Returns an entry from the history table
Input:		   Index of the history entry to return
Output:		   The history entry as a single line
Errors:
--------------------------------------------------------------------------
Edit History:
	15 Sept 2003 - Chris Watford watford@uiuc.edu
		- Complete rewrite
	17 Sept 2003 - Chris Watford watford@uiuc.edu
		- Added doubly link list support
------------------------------------------------------------------------*/
char *GetHistoryLine(int n)
{
	StatementHistory *histentry = History;
	int i;

	// traverse linked list looking for member n
	for (i = 0; ((i < n) && (histentry != NULL)); i++, histentry = histentry->Next);

	// figure out what to return
	if (histentry != NULL)
	{
		return editbuffer_getasline(histentry->Statement);
	} else {
		return "";
	}
}
