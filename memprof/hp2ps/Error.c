#include <stdio.h>
#include "Main.h"
#include "Defines.h"

/* own stuff */
#include "Error.h"

void exit PROTO((int));

/*VARARGS0*/
void
Error(a1,a2,a3,a4)
  char* a1; char* a2; char* a3; char* a4;
{
    fflush(stdout);
    fprintf(stderr, "%s: ", programname);
    fprintf(stderr, a1, a2, a3, a4);
    fprintf(stderr, "\n");
    exit(1);
}

/*VARARGS0*/
void
Disaster(a1,a2,a3,a4)
  char* a1; char* a2; char* a3; char* a4;
{
    fflush(stdout);
    fprintf(stderr, "%s: ", programname);
    fprintf(stderr, " Disaster! ("); 
    fprintf(stderr, a1, a2, a3, a4);
    fprintf(stderr, ")\n");
    exit(1);
}

void
Usage(str)
  char *str;
{
   if (str) printf("error: %s\n", str);
   printf("usage: %s -b -d -ef -g -i -p -mn -p -s -tf -y [file[.hp]]\n", programname);
   printf("where -b  use large title box\n");
   printf("      -d  sort by standard deviation\n"); 
   printf("      -ef[in|mm|pt] produce Encapsulated PostScript f units wide (f > 2 inches)\n");
   printf("      -g  produce output suitable for GHOSTSCRIPT previever\n");
   printf("      -i[+|-] sort by identifier string (-i+ gives greatest on top) \n"); 
   printf("      -mn print maximum of n bands (default & max 20)\n");
   printf("          -m0 removes the band limit altogether\n");
   printf("      -p  use previous scaling, shading and ordering\n");
   printf("      -s  use small title box\n");
   printf("      -tf ignore trace bands which sum below f%% (default 1%%, max 5%%)\n");
   printf("      -y  traditional\n");
   printf("      -c  colour ouput\n");
   exit(0);
}

