#ifndef UTILITIES_H
#define UTILITIES_H

char* Basename    PROTO((char *));
void  DropSuffix  PROTO((char *, char *));
FILE* OpenFile    PROTO((char *, char *));
void  CommaPrint  PROTO((FILE *, intish));
char *copystring  PROTO((char *));
char *copystring2 PROTO((char *, char *));
void *xmalloc	 PROTO((int));
void *xrealloc	 PROTO((void *, int));

#endif /* UTILITIES_H */
