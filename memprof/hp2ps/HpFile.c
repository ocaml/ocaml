#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "Main.h"
#include "Defines.h"
#include "Error.h"
#include "HpFile.h"
#include "Utilities.h"

#ifndef atof
double atof PROTO((const char *));
#endif

/* own stuff already included */

#define N_MARKS 50		/* start size of the mark table */
#define N_SAMPLES 500		/* start size of the sample table */

char *theident;
char *thestring;
int theinteger;
floatish thefloatish;
int ch;						/* last character read  */
token thetok; 					/* last token           */
int linenum;					/* current line number  */
int endfile;					/* true at end of file  */

static boolish gotjob = 0;			/* "JOB" read	        */
static boolish gotdate = 0;			/* "DATE" read          */
static boolish gotvalueunit = 0;		/* "VALUE_UNIT" read    */
static boolish gotsampleunit = 0;		/* "SAMPLE_UNIT" read   */
static boolish insample = 0;			/* true when in sample  */

static floatish lastsample;			/* the last sample time */

static void GetHpLine PROTO((FILE *));		/* forward */
static void GetHpTok  PROTO((FILE *));		/* forward */

static struct entry *GetEntry PROTO((char *));	/* forward */

static void MakeIdentTable PROTO((void));	/* forward */

char *jobstring;
char *datestring;

char *sampleunitstring;
char *valueunitstring;

floatish *samplemap;		/* sample intervals	*/
floatish *markmap;		/* sample marks		*/

/*
 *	An extremely simple parser. The input is organised into lines of 
 *	the form
 *
 *      JOB s              -- job identifier string
 *	DATE s		   -- date string 
 *	SAMPLE_UNIT s	   -- sample unit eg "seconds" 
 *	VALUE_UNIT s	   -- value unit eg "bytes" 
 *	MARK i	   	   -- sample mark 
 *	BEGIN_SAMPLE i 	   -- start of ith sample 
 *	identifier i	   -- there are i identifiers in this sample 
 *	END_SAMPLE i   	   -- end of ith sample 
 *
 */

void
GetHpFile(infp)
  FILE *infp;
{
    nsamples = 0;
    nmarks   = 0;
    nidents  = 0;

    ch = ' ';
    endfile = 0;
    linenum = 1;
    lastsample = 0.0;

    GetHpTok(infp);

    while (endfile == 0) {
	GetHpLine(infp);
    }

    if (!gotjob) {
	Error("%s: JOB missing", hpfile);
    }

    if (!gotdate) {
	Error("%s: DATE missing", hpfile);
    }

    if (!gotvalueunit) {
	Error("%s: VALUE_UNIT missing", hpfile);
    }

    if (!gotsampleunit) {
	Error("%s: SAMPLE_UNIT missing", hpfile);
    }

    if (nsamples == 0) {
	Error("%s: contains no samples", hpfile);
    }


    MakeIdentTable();

    fclose(hpfp);
}


/*
 *      Read the next line from the input, check the syntax, and perform
 *	the appropriate action.
 */

static void
GetHpLine(infp)
  FILE* infp;
{
    static intish nmarkmax = 0, nsamplemax = 0;

    switch (thetok) {
    case JOB_TOK:
	GetHpTok(infp);
	if (thetok != STRING_TOK) {
	    Error("%s, line %d: string must follow JOB", hpfile, linenum);
        }
	jobstring = thestring;
	gotjob = 1;
        GetHpTok(infp);
	break;

    case DATE_TOK:
	GetHpTok(infp);
	if (thetok != STRING_TOK) {
	    Error("%s, line %d: string must follow DATE", hpfile, linenum);
        }
	datestring = thestring;
	gotdate = 1;
        GetHpTok(infp);
	break;

    case SAMPLE_UNIT_TOK:
	GetHpTok(infp);
	if (thetok != STRING_TOK) {
	    Error("%s, line %d: string must follow SAMPLE_UNIT", hpfile, 
	          linenum);
        }
	sampleunitstring = thestring;
	gotsampleunit = 1;
        GetHpTok(infp);
	break;

    case VALUE_UNIT_TOK:
        GetHpTok(infp);
	if (thetok != STRING_TOK) {
	    Error("%s, line %d: string must follow VALUE_UNIT", hpfile, 
	          linenum);
        }
	valueunitstring = thestring;
	gotvalueunit = 1;
        GetHpTok(infp);
	break;

    case MARK_TOK:
	GetHpTok(infp);
        if (thetok != FLOAT_TOK) {
            Error("%s, line %d, floating point number must follow MARK",
	          hpfile, linenum);
        }
	if (insample) {
	    Error("%s, line %d, MARK occurs within sample", hpfile, linenum);
	}
	if (nmarks >= nmarkmax) {
	    if (!markmap) {
		nmarkmax = N_MARKS;
		markmap = (floatish*) xmalloc(nmarkmax * sizeof(floatish));
	    } else {
		nmarkmax *= 2;
		markmap = (floatish*) xrealloc(markmap, nmarkmax * sizeof(floatish));
	    }
	}
	markmap[ nmarks++ ] = thefloatish; 
        GetHpTok(infp);
        break;

    case BEGIN_SAMPLE_TOK: 
	insample = 1;
	GetHpTok(infp); 
	if (thetok != FLOAT_TOK) {
	    Error("%s, line %d, floating point number must follow BEGIN_SAMPLE",	          hpfile, linenum);
	}
	if (thefloatish < lastsample) {
	    Error("%s, line %d, samples out of sequence", hpfile, linenum);
	} else {
	    lastsample = thefloatish;
        }
	if (nsamples >= nsamplemax) {
	    if (!samplemap) {
		nsamplemax = N_SAMPLES;
		samplemap = (floatish*) xmalloc(nsamplemax * sizeof(floatish));
	    } else {
		nsamplemax *= 2;
		samplemap = (floatish*) xrealloc(samplemap, 
	                                      nsamplemax * sizeof(floatish));
	    }
	}
	samplemap[ nsamples ] = thefloatish;
	GetHpTok(infp);
	break;

    case END_SAMPLE_TOK: 
	insample = 0;
	GetHpTok(infp); 
	if (thetok != FLOAT_TOK) {
	    Error("%s, line %d: floating point number must follow END_SAMPLE", 
                  hpfile, linenum);
	}
        nsamples++;
	GetHpTok(infp);
	break;

    case IDENTIFIER_TOK:
	GetHpTok(infp);
	if (thetok != INTEGER_TOK) {
	    Error("%s, line %d: integer must follow identifier", hpfile, 
                  linenum);
	}
        StoreSample(GetEntry(theident), nsamples, (floatish) theinteger);
	GetHpTok(infp); 
        break;

    case EOF_TOK:
        endfile = 1;
	break;

    default:
	Error("%s, line %d: %s unexpected", hpfile, linenum,
	      TokenToString(thetok));
	break;
    }
}


char *
TokenToString(t)
  token t;
{
   switch (t) {
	case EOF_TOK:		return "EOF";
	case INTEGER_TOK:	return "integer";
	case FLOAT_TOK:		return "floating point number";
	case IDENTIFIER_TOK:	return "identifier";
	case STRING_TOK:	return "string";
	case BEGIN_SAMPLE_TOK:  return "BEGIN_SAMPLE";
	case END_SAMPLE_TOK:    return "END_SAMPLE";
	case JOB_TOK:		return "JOB";
	case DATE_TOK:		return "DATE";
	case SAMPLE_UNIT_TOK:   return "SAMPLE_UNIT";
	case VALUE_UNIT_TOK:    return "VALUE_UNIT";
	case MARK_TOK:		return "MARK";

	case X_RANGE_TOK:	return "X_RANGE";
	case Y_RANGE_TOK:	return "Y_RANGE";
	case ORDER_TOK:		return "ORDER";
	case SHADE_TOK:		return "SHADE";
        default:		return "(strange token)";
    }
}

/*
 *	Read the next token from the input and assign its value
 *	to the global variable "thetok". In the case of numbers,
 *	the corresponding value is also assigned to "theinteger"
 *	or "thefloatish" as appropriate; in the case of identifiers 
 *	it is assigned to "theident".
 */

static void
GetHpTok(infp)
  FILE* infp;
{

    while (isspace(ch)) {		/* skip whitespace */
	if (ch == '\n') linenum++;
	ch = getc(infp);
    } 

    if (ch == EOF) {
	thetok = EOF_TOK;
	return;
    }

    if (isdigit(ch)) {
	thetok = GetNumber(infp);
	return;
    } else if (ch == '\"') {
	GetString(infp);
	thetok = STRING_TOK;
	return;
    } else if (IsIdChar(ch)) {
	ASSERT(! (isdigit(ch)));	/* ch can't be a digit here */
	GetIdent(infp);
	if (!isupper(theident[0])) {
	    thetok = IDENTIFIER_TOK;
	} else if (strcmp(theident, "BEGIN_SAMPLE") == 0) {
            thetok = BEGIN_SAMPLE_TOK;
	} else if (strcmp(theident, "END_SAMPLE") == 0) {
            thetok = END_SAMPLE_TOK;
	} else if (strcmp(theident, "JOB") == 0) {
	    thetok = JOB_TOK;
	} else if (strcmp(theident, "DATE") == 0) {
	    thetok = DATE_TOK;
	} else if (strcmp(theident, "SAMPLE_UNIT") == 0) {
	    thetok = SAMPLE_UNIT_TOK;
	} else if (strcmp(theident, "VALUE_UNIT") == 0) {
	    thetok = VALUE_UNIT_TOK;
	} else if (strcmp(theident, "MARK") == 0) {
	    thetok = MARK_TOK;
	} else {
            thetok = IDENTIFIER_TOK;
	}
	return;
    } else {
	Error("%s, line %d: strange character (%c)", hpfile, linenum, ch);
    }
}


/*
 *	Read a sequence of digits and convert the result to an integer
 *	or floating point value (assigned to the "theinteger" or 
 *	"thefloatish").
 */

static char numberstring[ NUMBER_LENGTH - 1 ];

token
GetNumber(infp)
  FILE* infp;
{
    int i;
    int containsdot;
 
    ASSERT(isdigit(ch)); /* we must have a digit to start with */

    containsdot = 0;

    for (i = 0; i < NUMBER_LENGTH && (isdigit(ch) || ch == '.'); i++) {
        numberstring[ i ] = ch;
        containsdot |= (ch == '.'); 
        ch = getc(infp);
    }   
 
    ASSERT(i < NUMBER_LENGTH); /* did not overflow */

    numberstring[ i ] = '\0';
 
    if (containsdot) {
        thefloatish = (floatish) atof(numberstring);
	return FLOAT_TOK;
    } else {
	theinteger = atoi(numberstring);
	return INTEGER_TOK;
    }
}

/*
 *	Read a sequence of identifier characters and assign the result 
 *	to the string "theident".
 */

void
GetIdent(infp)
  FILE *infp;
{
    unsigned int i;
    char idbuffer[5000];

    for (i = 0; i < (sizeof idbuffer)-1 && IsIdChar(ch); i++) {
	idbuffer[ i ] = ch;
	ch = getc(infp);
    }
    
    idbuffer[ i ] = '\0';

    if (theident)
	free(theident);

    theident = copystring(idbuffer);
}


/*
 *	Read a sequence of characters that make up a string and 
 *	assign the result to "thestring".
 */

void
GetString(infp)
  FILE *infp;
{
    unsigned int i;
    char stringbuffer[5000];

    ASSERT(ch == '\"');

    ch = getc(infp);	/* skip the '\"' that begins the string */

    for (i = 0; i < (sizeof stringbuffer)-1 && ch != '\"'; i++) {
	stringbuffer[ i ] = ch;
	ch = getc(infp);
    }

    stringbuffer[i] = '\0'; 
    thestring = copystring(stringbuffer);

    ASSERT(ch == '\"');

    ch = getc(infp);      /* skip the '\"' that terminates the string */
}

boolish
IsIdChar(ch)
  int ch;
{
    return (!isspace(ch));
}


/*
 *      The information associated with each identifier is stored
 *	in a linked list of chunks. The table below allows the list
 *	of chunks to be retrieved given an identifier name.
 */

#define N_HASH       	513 

static struct entry* hashtable[ N_HASH ];

static intish
Hash(s)
  char *s;
{
    int r;
 
    for (r = 0; *s; s++) {
        r = r + r + r + *s;
    }

    if (r < 0) r = -r;

    return r % N_HASH;
}

/*
 *      Get space for a new chunk. Initialise it, and return a pointer 
 *	to the new chunk.
 */
 
static struct chunk*
MakeChunk()
{
    struct chunk* ch;
    struct datapoint* d;

    ch = (struct chunk*) xmalloc( sizeof(struct chunk) );
 
    d = (struct datapoint*) xmalloc (sizeof(struct datapoint) * N_CHUNK);

    ch->nd = 0; 
    ch->d = d;
    ch->next = 0;
    return ch;
}


/*
 *      Get space for a new entry. Initialise it, and return a pointer 
 *	to the new entry.
 */
 
struct entry *
MakeEntry(name)
  char *name;
{
    struct entry* e;

    e = (struct entry *) xmalloc(sizeof(struct entry));
    e->chk = MakeChunk();
    e->name = copystring(name); 
    return e;
}

/*
 *	Get the entry associated with "name", creating a new entry if 
 *	necessary.
 */

static struct entry *
GetEntry(name)
  char* name;
{
    intish h;
    struct entry* e;
 
    h = Hash(name);
 
    for (e = hashtable[ h ]; e; e = e->next) {
        if (strcmp(e->name, name) == 0) {
            break;
        }
    }
 
    if (e) {
	return (e); 
    } else {
        nidents++;
        e = MakeEntry(name);
        e->next = hashtable[ h ];
        hashtable[ h ] = e;
        return (e);
    }
}


/*
 *      Store information from a sample. 
 */
 
void
StoreSample(en, bucket, value)
  struct entry* en; intish bucket; floatish value;
{
    struct chunk* chk; 

    for (chk = en->chk; chk->next != 0; chk = chk->next)
	; 

    if (chk->nd < N_CHUNK) {
	chk->d[ chk->nd ].bucket = bucket;
	chk->d[ chk->nd ].value  = value;
	chk->nd += 1;
    } else {
	struct chunk* t;
	t = chk->next = MakeChunk(); 
	t->d[ 0 ].bucket = bucket;
	t->d[ 0 ].value  = value;
	t->nd += 1;
    }
}


struct entry** identtable;

/*
 *	The hash table is useful while reading the input, but it
 *	becomes a liability thereafter. The code below converts 
 *	it to a more easily processed table.
 */

static void
MakeIdentTable()
{
    intish i;
    intish j;
    struct entry* e;

    nidents = 0;
    for (i = 0; i < N_HASH; i++) {
        for (e = hashtable[ i ]; e; e = e->next) {
	    nidents++;
        }
    }

    identtable = (struct entry**) xmalloc(nidents * sizeof(struct entry*));
    j = 0;

    for (i = 0; i < N_HASH; i++) {
        for (e = hashtable[ i ]; e; e = e->next, j++) {
	    identtable[ j ] = e; 
        }
    }
}
