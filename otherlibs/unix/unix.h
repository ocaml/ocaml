#define Nothing ((value) 0)

#ifndef NULL
#ifdef ANSI
#define NULL ((void *) 0)
#else
#define NULL ((char *) 0)
#endif
#endif

#ifdef ANSI
extern void unix_error(int errcode, char * cmdname, value arg);
extern void uerror(char * cmdname, value arg);
#else
void unix_error();
void uerror();
#endif

