#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "Main.h"
#include "Defines.h"
#include "Error.h"
#include "Utilities.h"

/* own stuff */
#include "Shade.h"

static struct shade {
	char* ident;
	floatish shade;
} *shademap;

static int shademapmax = 0;
static int shademapindex = 0;

/*
 *	Set the shade to be used for "ident" to "shade".
 */

void
ShadeFor(ident, shade)
  char* ident; 
  floatish shade;
{
    if (! shademap) {
	shademapmax = (nidents > TWENTY ? nidents : TWENTY) * 2;
	         /* Assume nidents read is indication of the No of
		    idents in the .aux file (*2 for good luck) */
	         /* NB *2 is needed as .aux and .hp elements may differ */
	shademap = xmalloc(shademapmax * sizeof(struct shade));
    }

    if (shademapindex < shademapmax) {
	shademap[ shademapindex ].ident = copystring(ident);
	shademap[ shademapindex ].shade = shade;
	shademapindex++;
    } else {
	Disaster("shade map overflow");
    }
}

/*
 *	Get the shade to be used for "ident" if there is one. 
 *	Otherwise, think of a new one.
 */

static floatish ThinkOfAShade PROTO((void));	/* forward */

floatish
ShadeOf(ident)
  char* ident;
{
    int i;
    floatish shade;

    for (i = 0; i < shademapindex; i++) {
	if (strcmp(shademap[i].ident, ident) == 0) {	/* got it */
	    return(shademap[i].shade);
	}
    }

    shade = ThinkOfAShade();

    ShadeFor(ident, shade);

    return shade; 
}



#define N_MONO_SHADES 10 

static floatish m_shades[ N_MONO_SHADES ] = {
    0.00000, 0.20000, 0.60000, 0.30000, 0.90000, 
    0.40000, 1.00000, 0.70000, 0.50000,  0.80000
};

#define N_COLOUR_SHADES 27

/* HACK: 0.100505 means 100% red, 50% green, 50% blue */

static floatish c_shades[ N_COLOUR_SHADES ] = {
    0.000000, 0.000010, 0.001000, 0.001010, 0.100000,
    0.100010, 0.101000, 0.101010, 0.000005, 0.000500,
    0.000510, 0.001005, 0.050000, 0.050010, 0.051000,
    0.051010, 0.100005, 0.100500, 0.100510, 0.101005,
    0.000505, 0.050005, 0.050500, 0.050510, 0.051005,
    0.100505, 0.050505
};

static floatish
ThinkOfAShade()
{
    static int thisshade = -1;

    thisshade++;
    return cflag ?
	c_shades[ thisshade % N_COLOUR_SHADES ] :
	m_shades[ thisshade % N_MONO_SHADES   ] ;
}

static floatish
extract_colour(shade,factor)
  floatish shade;
  intish factor;
{
    intish i,j;

    i = (int)(shade * factor);
    j = i / 100;
    return (i - j * 100) / 10.0;
}

void
SetPSColour(shade)
  floatish shade;
{
    if (cflag) {
	fprintf(psfp, "%f %f %f setrgbcolor\n",
		extract_colour(shade, (intish)100),
		extract_colour(shade, (intish)10000),
		extract_colour(shade, (intish)1000000));
    } else {
	fprintf(psfp, "%f setgray\n", shade);
    }
}
