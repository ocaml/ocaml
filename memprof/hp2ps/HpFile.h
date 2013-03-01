#ifndef HP_FILE_H
#define HP_FILE_H

typedef enum {
        /* These tokens are found in ".hp" files */ 
 
	EOF_TOK,
	INTEGER_TOK,
	FLOAT_TOK,
	IDENTIFIER_TOK,
	STRING_TOK,
	BEGIN_SAMPLE_TOK,
	END_SAMPLE_TOK,
	JOB_TOK, 
	DATE_TOK,
	SAMPLE_UNIT_TOK,
	VALUE_UNIT_TOK,
	MARK_TOK,
 
	/* These extra ones are found only in ".aux" files */ 
 
	X_RANGE_TOK,
	Y_RANGE_TOK,
	ORDER_TOK,
	SHADE_TOK
} token;

struct datapoint {
    int bucket;
    floatish value;
};

struct chunk {
    struct chunk *next;
    short  nd;                          /* 0 .. N_CHUNK - 1 */
    struct datapoint *d;
};


struct entry {
    struct entry *next;
    struct chunk *chk;
    char   *name;
};

extern char *theident;
extern char *thestring;
extern int theinteger;
extern floatish thefloatish;
extern int ch;
extern token thetok;
extern int linenum; 
extern int endfile;

char *TokenToString PROTO((token));

extern struct entry** identtable;

extern floatish *samplemap;
extern floatish *markmap;

void GetHpFile PROTO((FILE *));
void StoreSample PROTO((struct entry *, intish, floatish));
struct entry *MakeEntry PROTO((char *));

token GetNumber PROTO((FILE *));
void  GetIdent  PROTO((FILE *));
void  GetString PROTO((FILE *));
boolish IsIdChar PROTO((int)); /* int is a "char" from getc */

extern char *jobstring;
extern char *datestring;
 
extern char *sampleunitstring;
extern char *valueunitstring;

#endif /* HP_FILE_H */
