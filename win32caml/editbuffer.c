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

/***********************************************************************/
/* Changes made by Chris Watford to enhance the source editor          */
/* Began 14 Sept 2003 - watford@uiuc.edu                               */
/***********************************************************************/

#include <string.h>
#include <stdlib.h>
#include "inriares.h"
#include "inria.h"

/*------------------------------------------------------------------------
 Procedure:     editbuffer_addline ID:1
 Author:		Chris Watford watford@uiuc.edu
 Purpose:       Adds a line to the current edit buffer
 Input:			Line of text to append to the end
 Output:
 Errors:
--------------------------------------------------------------------------
 Edit History:
	18 Sept 2003 - Chris Watford watford@uiuc.edu
		- Corrected doubly linked list issue
------------------------------------------------------------------------*/
BOOL editbuffer_addline(EditBuffer* edBuf, char* line)
{
	LineList *tail = NULL; //head of the edit buffer line list
	LineList *newline = NULL;

	// sanity check
	if(edBuf == NULL)
	{
		return FALSE;
	}

	// perform edit buffer sanity checks
	if((edBuf->LineCount < 0) || (edBuf->Lines == NULL))
	{
		edBuf->LineCount = 0;
	}

	// move to the end of the line list in the edit buffer
	if((tail = edBuf->Lines) != NULL)
		for( ; tail->Next != NULL; tail = tail->Next);

	// create the new line entry
	newline = (LineList*)SafeMalloc(sizeof(LineList));
	newline->Next = NULL;
	newline->Prev = tail;
	newline->Text = (char*)SafeMalloc(strlen(line)+1);
	strncpy(newline->Text, line, strlen(line)+1);
	newline->Text[strlen(line)] = '\0';

	// add it to the list as the head or the tail
	if(tail != NULL)
	{
		tail->Next = newline;
	} else {
		edBuf->Lines = newline;
	}

	// update the number of lines in the buffer
	edBuf->LineCount++;

	return TRUE;
}

/*------------------------------------------------------------------------
 Procedure:     editbuffer_updateline ID:1
 Author:		Chris Watford watford@uiuc.edu
 Purpose:       Updates the edit buffer's internal contents for a line
 Input:			idx - Line index
				line - String to add
 Output:		if the line was updated or not
 Errors:
------------------------------------------------------------------------*/
BOOL editbuffer_updateline(EditBuffer* edBuf, int idx, char* line)
{
	LineList *update = edBuf->Lines; //head of the edit buffer line list
	LineList *newline = NULL;
	int i;

	// sanity checks
	if(edBuf == NULL)
	{
		return FALSE;
	} else if(	(edBuf->LineCount == 0) ||
				(edBuf->Lines == NULL) ||
				(idx >= edBuf->LineCount) ||
				(idx < 0) ) {
		return FALSE;
	}

	// move to the index in the line list
	// i left in update != NULL as a sanity check
	for(i = 0; ((update != NULL) && (i != idx)); update = update->Next, i++);

	// did things mess up?
	if( (update == NULL) || (i != idx) )
	{
		return FALSE;
	}

	// get rid of the old line
	free(update->Text);

	// get the new line updated
	update->Text = (char*)SafeMalloc(strlen(line)+1);
	strncpy(update->Text, line, strlen(line)+1);
	update->Text[strlen(line)] = '\0';

	return TRUE;
}

/*------------------------------------------------------------------------
 Procedure:     editbuffer_updateoraddline ID:1
 Author:		Chris Watford watford@uiuc.edu
 Purpose:       Updates the edit buffer's internal contents for a line
 Input:			idx - Line index
				line - String to add
 Output:		if the line was updated or not
 Errors:
------------------------------------------------------------------------*/
BOOL editbuffer_updateoraddline(EditBuffer* edBuf, int idx, char* line)
{
	LineList *update;

	// sanity checks
	if(edBuf == NULL)
	{
		return FALSE;
	} else if((idx > edBuf->LineCount) || (idx < 0)) {
		return FALSE;
	}

	update = edBuf->Lines; //head of the edit buffer line list

	// do we update or add?
	if((idx < edBuf->LineCount) && (edBuf->Lines != NULL))
	{	//interior line, update
		return editbuffer_updateline(edBuf, idx, line);
	} else {
		//fence line, add
		return editbuffer_addline(edBuf, line);
	}
}

/*------------------------------------------------------------------------
 Procedure:     editbuffer_removeline ID:1
 Author:		Chris Watford watford@uiuc.edu
 Purpose:       Removes a line from the edit buffer
 Input:			idx - Line index to remove
 Output:		if the line was removed or not
 Errors:
--------------------------------------------------------------------------
 Edit History:
	18 Sept 2003 - Chris Watford watford@uiuc.edu
		- Added to allow backspace and delete support
		- Corrected doubly linked list issue
------------------------------------------------------------------------*/
BOOL editbuffer_removeline(EditBuffer* edBuf, int idx)
{
	LineList *update = NULL;
	int i = 0;

	// sanity checks
	if(edBuf == NULL)
	{
		return FALSE;
	} else if(	(edBuf->LineCount == 0) ||
				(edBuf->Lines == NULL) ||
				(idx >= edBuf->LineCount) ||
				(idx < 0) ) {
		return FALSE;
	}
	
	// move to the index in the line list
	// i left in update != NULL as a sanity check
	for(i = 0, update = edBuf->Lines; ((update != NULL) && (i != idx)); update = update->Next, i++);

	// remove this line
	if(update != NULL)
	{
		// break links, removing our line
		if(update->Prev != NULL)
		{
			// we're not the first so just break the link
			update->Prev->Next = update->Next;
			
			// fix the prev check
			if(update->Next != NULL)
				update->Next->Prev = update->Prev;
		} else {
			// we're the first, attach the next guy to lines
			edBuf->Lines = update->Next;
		}

		// one less line to worry about
		edBuf->LineCount--;

		// get rid of the text
		if(update->Text != NULL)
			free(update->Text);

		// get rid of us
		free(update);

		return TRUE;
	}

	return FALSE;
}

/*------------------------------------------------------------------------
 Procedure:     editbuffer_getasline ID:1
 Author:		Chris Watford watford@uiuc.edu
 Purpose:       Returns the edit buffer as one big line, \n's and \t's
				become spaces.
 Input:
 Output:
 Errors:
------------------------------------------------------------------------*/
char* editbuffer_getasline(EditBuffer* edBuf)
{
	LineList *line = NULL; //head of the edit buffer line list
	char* retline = (char*)realloc(NULL, 1);
	unsigned int i = 0;

	// fix retline bug
	retline[0] = '\0';

	// sanity checks
	if(edBuf == NULL)
	{
		return NULL;
	} else if (edBuf->LineCount == 0 || edBuf->Lines == NULL) {
		// fix any possible errors that may come from this
		edBuf->LineCount = 0;
		edBuf->Lines = NULL;
		return NULL;
	}

	// get the big line
	for(line = edBuf->Lines; line != NULL; line = line->Next)
	{
		if(line->Text != NULL)
		{
			retline = (char*)realloc(retline, (strlen(retline) + strlen(line->Text) + (strlen(retline) > 0 ? 2 : 1)));

			if(strlen(retline) > 0)
				retline = strcat(retline, " ");

			retline = strcat(retline, line->Text);

			//concat in the hoouuusssseee!
		}
	}

	// now we have the big line, so lets ditch all \n's \t's and \r's
	for(i = 0; i < strlen(retline); i++)
	{
		switch(retline[i])
		{
			case '\n':
			case '\t':
			case '\r':
				retline[i] = ' ';
		}
	}

	return retline;
}

/*------------------------------------------------------------------------
 Procedure:     editbuffer_getasbuffer ID:1
 Author:		Chris Watford watford@uiuc.edu
 Purpose:       Returns the edit buffer as one big line, \n's and \t's
				become spaces.
 Input:
 Output:
 Errors:
------------------------------------------------------------------------*/
char* editbuffer_getasbuffer(EditBuffer* edBuf)
{
	LineList *line = NULL; //head of the edit buffer line list
	char* retbuf = (char*)realloc(NULL, 1);
	unsigned int i = 0;

	// fix retline bug
	retbuf[0] = '\0';

	// sanity checks
	if(edBuf == NULL)
	{
		return NULL;
	} else if (edBuf->LineCount == 0 || edBuf->Lines == NULL) {
		// fix any possible errors that may come from this
		edBuf->LineCount = 0;
		edBuf->Lines = NULL;
		return NULL;
	}

	// get the big line
	for(line = edBuf->Lines; line != NULL; line = line->Next)
	{
		if(line->Text != NULL)
		{
			int len = strlen(retbuf);
			len += strlen(line->Text) + (len > 0 ? 3 : 1);

			retbuf = (char*)realloc(retbuf, len);

			if(strlen(retbuf) > 0)
				retbuf = strcat(retbuf, "\r\n");

			retbuf = strcat(retbuf, line->Text);

			retbuf[len-1] = '\0';

			//concat in the hoouuusssseee!
		}
	}

	return retbuf;
}

/*------------------------------------------------------------------------
 Procedure:     editbuffer_lastline ID:1
 Author:		Chris Watford watford@uiuc.edu
 Purpose:       Returns the last line in the edit buffer
 Input:
 Output:
 Errors:
------------------------------------------------------------------------*/
char* editbuffer_lastline(EditBuffer* edBuf)
{
	LineList *line = NULL; //head of the edit buffer line list

	// sanity checks
	if(edBuf == NULL)
	{
		return NULL;
	} else if (edBuf->LineCount == 0 || edBuf->Lines == NULL) {
		// fix any possible errors that may come from this
		edBuf->LineCount = 0;
		edBuf->Lines = NULL;
		return NULL;
	}

	// go to the last line
	for(line = edBuf->Lines; line->Next != NULL; line = line->Next);

	return line->Text;
}

/*------------------------------------------------------------------------
 Procedure:     editbuffer_copy ID:1
 Author:		Chris Watford watford@uiuc.edu
 Purpose:       Makes an exact copy of an edit buffer
 Input:
 Output:
 Errors:
--------------------------------------------------------------------------
 Edit History:
	16 Sept 2003 - Chris Watford watford@uiuc.edu
		- Added to make copies of history entries
	18 Sept 2003 - Chris Watford watford@uiuc.edu
		- Corrected doubly linked list issue
	06 Oct  2003 - Chris Watford watford@uiuc.edu
		- Added isCorrect flag
------------------------------------------------------------------------*/
EditBuffer* editbuffer_copy(EditBuffer* edBuf)
{
	// sanity checks
	if(edBuf == NULL)
	{
		return NULL;
	} else {
		EditBuffer* copy = (EditBuffer*)SafeMalloc(sizeof(EditBuffer));
		LineList* lines = edBuf->Lines;
		LineList* lastLine = NULL;

		// clear its initial values
		copy->LineCount = 0;
		copy->Lines = NULL;
		copy->isCorrect = FALSE;

		// well we don't have to copy much
		if((lines == NULL) || (edBuf->LineCount <= 0))
		{
			return copy;
		}

		// get if its correct
		copy->isCorrect = edBuf->isCorrect;

		// go through each line, malloc it and add it
		for( ; lines != NULL; lines = lines->Next)
		{
			LineList* curline = (LineList*)SafeMalloc(sizeof(LineList));
			curline->Next = NULL;
			curline->Prev = NULL;

			// if there was a last line, link them to us
			if(lastLine != NULL)
			{
				lastLine->Next = curline;
				curline->Prev = lastLine;
			}

			// are we the first line? add us to the edit buffer as the first
			if(copy->Lines == NULL)
			{
				copy->Lines = curline;
			}

			// check if there is text on the line
			if(lines->Text == NULL)
			{	// no text, make it blankz0r
				curline->Text = (char*)SafeMalloc(sizeof(char));
				curline->Text[0] = '\0';
			} else {
				// there is text, copy it and null-terminate
				curline->Text = (char*)SafeMalloc(strlen(lines->Text) + 1);
				strncpy(curline->Text, lines->Text, strlen(lines->Text));
				curline->Text[strlen(lines->Text)] = '\0';
			}

			// up the line count and make us the last line
			copy->LineCount++;
			lastLine = curline;
		}

		// return our new copy
		return copy;
	}
}

/*------------------------------------------------------------------------
 Procedure:     editbuffer_destroy ID:1
 Author:		Chris Watford watford@uiuc.edu
 Purpose:       Destroys an edit buffer
 Input:
 Output:
 Errors:
------------------------------------------------------------------------*/
void editbuffer_destroy(EditBuffer* edBuf)
{
	// sanity checks
	if(edBuf == NULL)
	{	// nothing to do
		return;
	} else if(edBuf->Lines != NULL) {
		LineList* lastline = NULL;

		// loop through each line free'ing its text
		for( ; edBuf->Lines != NULL; edBuf->Lines = edBuf->Lines->Next)
		{
			if(edBuf->Lines->Text != NULL)
				free(edBuf->Lines->Text);

			// if there was a line before us, free it
			if(lastline != NULL)
			{
				free(lastline);
				lastline = NULL;
			}

			lastline = edBuf->Lines;
		}

		// free the last line
		free(lastline);
	}

	// free ourself
	free(edBuf);
}

/*------------------------------------------------------------------------
 Procedure:     editbuffer_new ID:1
 Author:		Chris Watford watford@uiuc.edu
 Purpose:       Creates an edit buffer
 Input:
 Output:
 Errors:
--------------------------------------------------------------------------
 Edit History:
	06 Oct  2003 - Chris Watford watford@uiuc.edu
		- Added isCorrect flag
------------------------------------------------------------------------*/
EditBuffer* editbuffer_new(void)
{
	// create a new one
	EditBuffer *edBuf = (EditBuffer*)SafeMalloc(sizeof(EditBuffer));
	
	// default vals
	edBuf->LineCount = 0;
	edBuf->Lines = NULL;
	edBuf->isCorrect = FALSE;
	
	// return it
	return edBuf;
}
