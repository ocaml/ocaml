#ifndef MAIN_H
#define MAIN_H

/* #include "config.h" */

#ifdef __STDC__
#define PROTO(x)	x
#else
#define PROTO(x)	()
#endif

/* our own ASSERT macro (for C) */
#ifndef DEBUG
#define ASSERT(predicate) /*nothing*/

#else
void _ghcAssert PROTO((char *, unsigned int));

#define ASSERT(predicate)			\
	if (predicate)				\
	    /*null*/;				\
	else					\
	    _ghcAssert(__FILE__, __LINE__)
#endif

/* partain: some ubiquitous types: floatish & intish.
   Dubious to use float/int, but that is what it used to be...
   (WDP 95/03)   
*/
typedef double	floatish;
typedef double  doublish; /* higher precision, if anything; little used */
typedef int	boolish;

/* Use "long long" if we have it: the numbers in profiles can easily
 * overflow 32 bits after a few seconds execution.
 */
#ifdef HAVE_LONG_LONG
typedef long long int intish;
#else
typedef long int intish;
#endif

extern intish nsamples;
extern intish nmarks;
extern intish nidents;

extern floatish maxcombinedheight;
extern floatish areabelow;
extern floatish epsfwidth;

extern floatish xrange;
extern floatish yrange;

extern floatish auxxrange;
extern floatish auxyrange;

extern boolish eflag;
extern boolish gflag;
extern boolish yflag;
extern boolish bflag;
extern boolish sflag;
extern int     mflag;
extern boolish tflag;
extern boolish cflag;

extern char *programname;

extern char *hpfile;
extern char *psfile;
extern char *auxfile;

extern FILE *hpfp;
extern FILE *psfp;
extern FILE *auxfp;

#endif /* MAIN_H */
