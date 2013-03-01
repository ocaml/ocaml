#include <stdio.h>
#include <math.h>
#include "Main.h"
#include "Defines.h"
#include "Dimensions.h"
#include "HpFile.h"
#include "Shade.h"
#include "Utilities.h"

/* own stuff */
#include "Curves.h"

static floatish *x;		/* x and y values  */
static floatish *y;

static floatish *py;		/* previous y values */

static void Curve PROTO((struct entry *));	/* forward */
static void ShadeCurve();			/* forward */

void
Curves()
{
    intish i;

    for (i = 0; i < nidents; i++) {
        Curve(identtable[i]);
    }
}

/*
 *      Draw a curve, and fill the area that is below it and above 
 *	the previous curve.
 */

static void
Curve(e)
  struct entry* e;
{
    struct chunk* ch;
    int j;
  
    for (ch = e->chk; ch; ch = ch->next) {
        for (j = 0; j < ch->nd; j++) {
	    y[ ch->d[j].bucket ] += ch->d[j].value;
	}
    }    

    ShadeCurve(x, y, py, ShadeOf(e->name));
}


static void PlotCurveLeftToRight PROTO((floatish *, floatish *)); /* forward */
static void PlotCurveRightToLeft PROTO((floatish *, floatish *)); /* forward */

static void SaveCurve PROTO((floatish *, floatish *)); /* forward */

/*
 *	Map virtual x coord to physical x coord 
 */
 
floatish
xpage(x)
  floatish x;
{
    return (x + graphx0); 
}



/*
 *	Map virtual y coord to physical y coord 
 */
 
floatish
ypage(y)
  floatish y;
{
    return (y + graphy0); 
}


/*
 *	Fill the region bounded by two splines, using the given 
 *	shade.
 */

static void
ShadeCurve(x, y, py, shade)
  floatish *x; floatish *y; floatish *py; floatish shade;
{
    fprintf(psfp, "%f %f moveto\n", xpage(x[0]), ypage(py[0]));
    PlotCurveLeftToRight(x, py);

    fprintf(psfp, "%f %f lineto\n", xpage(x[nsamples - 1]), 
                                    ypage(y[nsamples - 1]));
    PlotCurveRightToLeft(x, y);

    fprintf(psfp, "closepath\n");

    fprintf(psfp, "gsave\n");

    SetPSColour(shade);
    fprintf(psfp, "fill\n");

    fprintf(psfp, "grestore\n");
    fprintf(psfp, "stroke\n");

    SaveCurve(y, py);
}

static void
PlotCurveLeftToRight(x,y)
  floatish *x; floatish *y;
{
    intish i;

    for (i = 0; i < nsamples; i++) {
        fprintf(psfp, "%f %f lineto\n", xpage(x[i]), ypage(y[i]));
    }
}

static void
PlotCurveRightToLeft(x,y)
  floatish *x; floatish *y;
{
    intish i;

    for (i = nsamples - 1; i >= 0; i-- ) {
        fprintf(psfp, "%f %f lineto\n", xpage(x[i]), ypage(y[i]));
    }
}

/*
 *	Save the curve coordinates stored in y[] in py[].
 */

static void
SaveCurve(y, py)
  floatish *y; floatish* py;
{
    intish i;

    for (i = 0; i < nsamples; i++) {
	py[i] = y[i];
    }
}

extern floatish xrange;

void
CurvesInit()
{
    intish i;

    x  =  (floatish*) xmalloc(nsamples * sizeof(floatish));
    y  =  (floatish*) xmalloc(nsamples * sizeof(floatish));
    py =  (floatish*) xmalloc(nsamples * sizeof(floatish));

    for (i = 0; i < nsamples; i++) {
        x[i] = ((samplemap[i] - samplemap[0])/ xrange) * graphwidth;
        y[i] = py[i] = 0.0; 
    }
}
