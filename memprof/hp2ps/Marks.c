#include <stdio.h>
#include "Main.h"
#include "Curves.h"
#include "Dimensions.h"
#include "HpFile.h"

/* own stuff */
#include "Marks.h"

static void Caret PROTO((floatish, floatish, floatish));

void
Marks()
{
    intish i;
    floatish m;

    for (i = 0; i < nmarks; i++) {
	m = ((markmap[i] - samplemap[0]) / xrange) * graphwidth;
	Caret(xpage(m), ypage(0.0), 4.0);
    }
}


/*
 * Draw a small white caret at (x,y) with width 2 * d
 */

static void
Caret(x,y,d)
  floatish x; floatish y; floatish d;
{
    fprintf(psfp, "%f %f moveto\n", x - d, y);
    fprintf(psfp, "%f %f rlineto\n",  d, -d);
    fprintf(psfp, "%f %f rlineto\n",  d,  d);
    fprintf(psfp, "closepath\n");

    fprintf(psfp, "gsave\n");
    fprintf(psfp, "1.0 setgray\n");
    fprintf(psfp, "fill\n");
    fprintf(psfp, "grestore\n");
    fprintf(psfp, "stroke\n");
}
