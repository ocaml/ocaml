#include <stdio.h>
#include <string.h>
#include "Main.h"
#include "Curves.h"
#include "Defines.h"
#include "Dimensions.h"
#include "HpFile.h"
#include "Utilities.h"

/* own stuff */
#include "Axes.h"

typedef enum {MEGABYTE, KILOBYTE, BYTE} mkb; 

static void XAxis PROTO((void)); /* forward */
static void YAxis PROTO((void)); /* forward */

static void XAxisMark PROTO((floatish, floatish));      /* forward */
static void YAxisMark PROTO((floatish, floatish, mkb)); /* forward */

static floatish Round PROTO((floatish)); /* forward */

void
Axes()
{
    XAxis();
    YAxis();
}

static void
XAxisMark(x, num)
  floatish x; floatish num;
{
    /* calibration mark */
    fprintf(psfp, "%f %f moveto\n", xpage(x), ypage(0.0));
    fprintf(psfp, "0 -4 rlineto\n");
    fprintf(psfp, "stroke\n");

    /* number */
    fprintf(psfp, "HE%d setfont\n", NORMAL_FONT);
    fprintf(psfp, "(%.1f)\n", num);
    fprintf(psfp, "dup stringwidth pop\n");
    fprintf(psfp, "2 div\n");
    fprintf(psfp, "%f exch sub\n", xpage(x));
    fprintf(psfp, "%f moveto\n", borderspace);
    fprintf(psfp, "show\n");
}


#define N_X_MARKS   	 7
#define XFUDGE   	15	

extern floatish xrange;
extern char *sampleunitstring;

static void
XAxis()
{
    floatish increment, i; 
    floatish t, x;
    floatish legendlen;
 
    /* draw the x axis line */
    fprintf(psfp, "%f %f moveto\n", xpage(0.0), ypage(0.0));
    fprintf(psfp, "%f 0 rlineto\n", graphwidth);
    fprintf(psfp, "%f setlinewidth\n", borderthick);
    fprintf(psfp, "stroke\n"); 

    /* draw x axis legend */
    fprintf(psfp, "HE%d setfont\n", NORMAL_FONT);
    fprintf(psfp, "(%s)\n", sampleunitstring);
    fprintf(psfp, "dup stringwidth pop\n");
    fprintf(psfp, "%f\n", xpage(0.0) + graphwidth);
    fprintf(psfp, "exch sub\n");
    fprintf(psfp, "%f moveto\n", borderspace);
    fprintf(psfp, "show\n");


    /* draw x axis scaling */

    increment = 1;/* Round(xrange / (floatish) N_X_MARKS); */

    t = graphwidth / xrange;
    legendlen = StringSize(sampleunitstring) + (floatish) XFUDGE;
 
    for (i = samplemap[0]; i < samplemap[nsamples - 1]; i += increment) {
        x = (i - samplemap[0]) * t;  
 
        if (x < (graphwidth - legendlen)) { 
            XAxisMark(x,i);
        } 
    } 
}

static void
YAxisMark(y, num, unit)
  floatish y; floatish num; mkb unit;
{
    /* calibration mark */
    fprintf(psfp, "%f %f moveto\n", xpage(0.0), ypage(y));
    fprintf(psfp, "-4 0 rlineto\n");
    fprintf(psfp, "stroke\n");
 
    /* number */
    fprintf(psfp, "HE%d setfont\n", NORMAL_FONT);

    switch (unit) {
    case MEGABYTE :
	fprintf(psfp, "(");
	CommaPrint(psfp, (intish) (num / 1e6 + 0.5));
	fprintf(psfp, "M)\n");
	break;
    case KILOBYTE :
	fprintf(psfp, "(");
	CommaPrint(psfp, (intish) (num / 1e3 + 0.5));
	fprintf(psfp, "k)\n");
	break;
    case BYTE:
	fprintf(psfp, "(");
	CommaPrint(psfp, (intish) (num + 0.5));
	fprintf(psfp, ")\n");
	break;
    }

    fprintf(psfp, "dup stringwidth\n");
    fprintf(psfp, "2 div\n");
    fprintf(psfp, "%f exch sub\n", ypage(y));

    fprintf(psfp, "exch\n");
    fprintf(psfp, "%f exch sub\n", graphx0 - borderspace);

    fprintf(psfp, "exch\n");
    fprintf(psfp, "moveto\n");
    fprintf(psfp, "show\n");
}

#define N_Y_MARKS 	 7	
#define YFUDGE 		15 

extern floatish yrange;
extern char *valueunitstring;

static void
YAxis()
{
    floatish increment, i;
    floatish t, y;
    floatish legendlen;
    mkb unit;

    /* draw the y axis line */
    fprintf(psfp, "%f %f moveto\n", xpage(0.0), ypage(0.0));
    fprintf(psfp, "0 %f rlineto\n", graphheight);
    fprintf(psfp, "%f setlinewidth\n", borderthick);
    fprintf(psfp, "stroke\n");

    /* draw y axis legend */
    fprintf(psfp, "gsave\n");
    fprintf(psfp, "HE%d setfont\n", NORMAL_FONT);
    fprintf(psfp, "(%s)\n", valueunitstring);
    fprintf(psfp, "dup stringwidth pop\n");
    fprintf(psfp, "%f\n", ypage(0.0) + graphheight);
    fprintf(psfp, "exch sub\n");
    fprintf(psfp, "%f exch\n", xpage(0.0) - borderspace);
    fprintf(psfp, "translate\n");
    fprintf(psfp, "90 rotate\n");
    fprintf(psfp, "0 0 moveto\n");
    fprintf(psfp, "show\n");
    fprintf(psfp, "grestore\n");

    /* draw y axis scaling */
    increment = max( yrange / (floatish) N_Y_MARKS, 1.0);
    increment = Round(increment);

    if (increment >= 1e6) {
	unit = MEGABYTE;
    } else if (increment >= 1e3) {
	unit = KILOBYTE;
    } else {
	unit = BYTE;
    }	

    t = graphheight / yrange; 
    legendlen = StringSize(valueunitstring) + (floatish) YFUDGE; 

    for (i = 0.0; i <= yrange; i += increment) {
        y = i * t;

        if (y < (graphheight - legendlen)) {
            YAxisMark(y, i, unit);
        }
    } 
}


/*
 *      Find a "nice round" value to use on the axis.
 */

static floatish OneTwoFive PROTO((floatish)); /* forward */

static floatish
Round(y)
  floatish y;
{
    int i;

    if (y > 10.0) {
	for (i = 0; y > 10.0; y /= 10.0, i++) ;
	y = OneTwoFive(y);
	for ( ; i > 0; y = y * 10.0, i--) ;

    } else if (y < 1.0) {
	for (i = 0; y < 1.0; y *= 10.0, i++) ;
        y = OneTwoFive(y);
        for ( ; i > 0; y = y / 10.0, i--) ;
 
    } else {
	y = OneTwoFive(y);
    }

    return (y);
}


/*
 * OneTwoFive() -- Runciman's 1,2,5 scaling rule. Argument  1.0 <= y <= 10.0.
 */

static floatish
OneTwoFive(y)
  floatish y;
{
    if (y > 4.0) {
	return (5.0);
    } else if (y > 1.0) {
	return (2.0);
    } else {
	return (1.0);
    }   
}
