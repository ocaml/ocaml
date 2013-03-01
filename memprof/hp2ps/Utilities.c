#include <stdio.h>
#include <string.h>
#include "Main.h"
#include "Error.h"

extern void* malloc();

char*
Basename(name)
  char* name;
{
    char* t;

    t = name;

    while (*name) {
        if (*name == '/') {
            t = name+1;
        }
        name++;
    }

    return t;
}

void
DropSuffix(name, suffix)
  char* name; char* suffix;
{
    char* t;

    t = (char*) 0;

    while (*name) {
	if (*name == '.') {
	     t = name;
	}
	name++;
    }

    if (t != (char*) 0 && strcmp(t, suffix) == 0) {
	*t = '\0';
    }
}

FILE*
OpenFile(s, mode)
  char* s; char* mode;
{
    FILE* r;

    if ((r = fopen(s, mode)) == NULL) {
	/*NOTREACHED*/
	Error("cannot open %s", s);
    }

    return r;
}


#define ONETHOUSAND     1000

/*
 *	Print a positive integer with commas
 */

void
CommaPrint(fp,n)
  FILE* fp;
  intish n;
{
    if (n < ONETHOUSAND) {
        fprintf(fp, "%d", (int)n);
    } else {
        CommaPrint(fp, n / ONETHOUSAND);
        fprintf(fp, ",%03d", (int)n % ONETHOUSAND);
    }
}

void *
xmalloc(n)
  int n;
{
    void *r;

    r = (void*) malloc(n);
    if (!r) {
	/*NOTREACHED*/
	Disaster("%s, sorry, out of memory", hpfile);
    }
    return r;
}

void *
xrealloc(p, n)
  void *p;
  int n;
{
    void *r;
    extern void *realloc();

    r = realloc(p, n);
    if (!r) {
	/*NOTREACHED*/
	Disaster("%s, sorry, out of memory", hpfile);
    }
    return r;
}

char *
copystring(s)
  char *s;
{
    char *r;

    r = (char*) xmalloc(strlen(s)+1);
    strcpy(r, s);
    return r;
}

char *
copystring2(s, t)
  char *s, *t;
{
    char *r;

    r = (char*) xmalloc(strlen(s)+strlen(t)+1);
    strcpy(r, s);
    strcat(r, t);
    return r;
}

