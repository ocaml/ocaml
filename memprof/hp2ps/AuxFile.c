#include <ctype.h>
#include <stdio.h>
#include <string.h>
#include "Main.h"
#include "Defines.h"
#include "Shade.h"
#include "Error.h"
#include "HpFile.h"
#include "Reorder.h"

/* own stuff */
#include "AuxFile.h"

static void GetAuxLine PROTO((FILE *));	/* forward */
static void GetAuxTok  PROTO((FILE *));	/* forward */

void
GetAuxFile(auxfp)
  FILE* auxfp;
{
    ch = ' ';
    endfile = 0;
    linenum = 1;
 
    GetAuxTok(auxfp);
 
    while (endfile == 0) {
        GetAuxLine(auxfp);
    }

    fclose(auxfp);
}



/*
 *      Read the next line from the aux file, check the syntax, and 
 *	perform the appropriate action.
 */

static void
GetAuxLine(auxfp)
  FILE* auxfp;
{
    switch (thetok) {
    case X_RANGE_TOK:
	GetAuxTok(auxfp);
	if (thetok != FLOAT_TOK) {
	    Error("%s, line %d, floating point number must follow X_RANGE", 
                  auxfile, linenum);
	}
	auxxrange = thefloatish;
        GetAuxTok(auxfp);
	break;
    case Y_RANGE_TOK:
	GetAuxTok(auxfp);
	if (thetok != FLOAT_TOK) {
	    Error("%s, line %d, floating point number must follow Y_RANGE", 
                  auxfile, linenum);
	}
	auxyrange = thefloatish;
        GetAuxTok(auxfp);
	break;
    case ORDER_TOK:
	GetAuxTok(auxfp);
	if (thetok != IDENTIFIER_TOK) {
            Error("%s, line %d: identifier must follow ORDER",
                  auxfile, linenum);
        }
	GetAuxTok(auxfp);
        if (thetok != INTEGER_TOK) {
            Error("%s, line %d: identifier and integer must follow ORDER",
                  auxfile, linenum);
        }
	OrderFor(theident, theinteger);
	GetAuxTok(auxfp);
        break;
    case SHADE_TOK:
	GetAuxTok(auxfp);
	if (thetok != IDENTIFIER_TOK) {
	    Error("%s, line %d: identifier must follow SHADE", 
                  auxfile, linenum);
	}
	GetAuxTok(auxfp);
	if (thetok != FLOAT_TOK) {
	    Error("%s, line %d: identifier and floating point number must follow SHADE",
	          auxfile, linenum);
	}
        ShadeFor(theident, thefloatish);
	GetAuxTok(auxfp); 
        break;
    case EOF_TOK:
        endfile = 1;
	break;
    default:
	Error("%s, line %d: %s unexpected", auxfile, linenum,
	      TokenToString(thetok));
	break;
    }
}



/*
 *      Read the next token from the input and assign its value
 *      to the global variable "thetok". In the case of numbers,
 *      the corresponding value is also assigned to "thefloatish"; 
 * 	in the case of identifiers it is assigned to "theident".
 */
 
static void GetAuxTok(auxfp)
FILE* auxfp;
{

    while (isspace(ch)) {               /* skip whitespace */
        if (ch == '\n') linenum++;
        ch = getc(auxfp);
    } 

    if (ch == EOF) {
        thetok = EOF_TOK;
        return;
    }

    if (isdigit(ch)) {
        thetok = GetNumber(auxfp);
        return;
    } else if (IsIdChar(ch)) {          /* ch can't be a digit here */
        GetIdent(auxfp);
	if (!isupper(theident[0])) {
            thetok = IDENTIFIER_TOK;
        } else if (strcmp(theident, "X_RANGE") == 0) {
            thetok = X_RANGE_TOK;
        } else if (strcmp(theident, "Y_RANGE") == 0) {
            thetok = Y_RANGE_TOK;
        } else if (strcmp(theident, "ORDER") == 0) {
            thetok = ORDER_TOK;
        } else if (strcmp(theident, "SHADE") == 0) {
            thetok = SHADE_TOK;
        } else {
            thetok = IDENTIFIER_TOK;
        }
        return;
    } else {
        Error("%s, line %d: strange character (%c)", auxfile, linenum, ch);
    }
}

void
PutAuxFile(auxfp)
  FILE* auxfp;
{
    int i;

    fprintf(auxfp, "X_RANGE %.2f\n", xrange);
    fprintf(auxfp, "Y_RANGE %.2f\n", yrange);

    for (i = 0; i < nidents; i++) {
        fprintf(auxfp, "ORDER %s %d\n", identtable[i]->name, i+1);
    }

    for (i = 0; i < nidents; i++) {
        fprintf(auxfp, "SHADE %s %.2f\n", identtable[i]->name, 
                       ShadeOf(identtable[i]->name));
    }

    fclose(auxfp);
}
