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

#ifndef _EDITBUFFER_H_
#define _EDITBUFFER_H_

// All the below was added by Chris Watford watford@uiuc.edu

typedef struct tagLineList {
        struct tagLineList	*Next;
		struct tagLineList	*Prev;
        char				*Text;
} LineList;

typedef struct tagEditBuffer {
	int					LineCount;
	struct tagLineList	*Lines;
	BOOL				isCorrect;
} EditBuffer;

BOOL		editbuffer_addline			(EditBuffer* edBuf, char* line);
BOOL		editbuffer_updateline		(EditBuffer* edBuf, int idx, char* line);
BOOL		editbuffer_updateoraddline	(EditBuffer* edBuf, int idx, char* line);
BOOL		editbuffer_removeline		(EditBuffer* edBuf, int idx);
char*		editbuffer_getasline		(EditBuffer* edBuf);
char*		editbuffer_getasbuffer		(EditBuffer* edBuf);
char*		editbuffer_lastline			(EditBuffer* edBuf);
EditBuffer*	editbuffer_copy				(EditBuffer* edBuf);
void		editbuffer_destroy			(EditBuffer* edBuf);
EditBuffer*	editbuffer_new				(void);

#endif
