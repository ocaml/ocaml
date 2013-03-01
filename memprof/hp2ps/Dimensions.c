#include <ctype.h>
#include <string.h>
#include <stdio.h>
#include "Main.h"
#include "Defines.h"
#include "HpFile.h"
#include "Scale.h"

/* own stuff */
#include "Dimensions.h"

/*
 *	Get page and other dimensions before printing.
 */

floatish borderheight   = BORDER_HEIGHT;
floatish borderwidth    = BORDER_WIDTH;
floatish borderspace    = BORDER_SPACE;
floatish borderthick    = BORDER_THICK;

floatish titlewidth     = (BORDER_WIDTH  - (2 * BORDER_SPACE)); 
floatish titletextspace = TITLE_TEXT_SPACE;
floatish titleheight; 

floatish graphx0 = GRAPH_X0;
floatish graphy0 = GRAPH_Y0;

floatish graphheight;
floatish graphwidth;

static floatish KeyWidth PROTO((void)); /* forward */

void
Dimensions()
{
    xrange = samplemap[nsamples - 1] - samplemap[0];
    xrange = max(xrange, auxxrange);
    if (xrange == 0.0) xrange = 1.0;            /* avoid division by 0.0 */
 
    yrange = MaxCombinedHeight();
    yrange = max(yrange, auxyrange);
    if (yrange == 0.0) yrange = 1.0;            /* avoid division by 0.0 */

    if (!bflag && !sflag) {
	bflag = strlen(jobstring) > SMALL_JOB_STRING_WIDTH; 
    }

    if (bflag) {
	titleheight = 2 * TITLE_HEIGHT;
    } else {
	titleheight = TITLE_HEIGHT;
    } 

    graphwidth  = titlewidth - graphx0 - (TWENTY ? KeyWidth() : 0);
    graphheight = borderheight - titleheight - (2 * borderspace) - graphy0;
}

/*
 *	Calculate the width of the key.
 */

static floatish
KeyWidth()
{
    intish i;
    floatish c;

    c = 0.0;

    for (i = 0; i < nidents; i++) {
        c = max(c, StringSize(identtable[i]->name));
    }

    c += 3.0 * borderspace;

    c += (floatish) KEY_BOX_WIDTH;

    return c;
}


/*
 *	A desperately grim solution.
 */


floatish fonttab[] = {
    /*  20 (' ') = */ 3.0,
    /*  21 ('!') = */ 1.0,
    /*  22 ('"') = */ 1.0,
    /*  23 ('#') = */ 3.0,
    /*  24 ('$') = */ 3.0,
    /*  25 ('%') = */ 3.0,
    /*  26 ('&') = */ 3.0,
    /*  27 (''') = */ 1.0,
    /*  28 ('(') = */ 3.0,
    /*  29 (')') = */ 3.0,
    /*  2a ('*') = */ 2.0,
    /*  2b ('+') = */ 3.0,
    /*  2c (',') = */ 1.0,
    /*  2d ('-') = */ 3.0,
    /*  2e ('.') = */ 1.0,
    /*  2f ('/') = */ 3.0,
    /*  30 ('0') = */ 4.0,
    /*  31 ('1') = */ 4.0,
    /*  32 ('2') = */ 4.0,
    /*  33 ('3') = */ 4.0,
    /*  34 ('4') = */ 4.0,
    /*  35 ('5') = */ 4.0,
    /*  36 ('6') = */ 4.0,
    /*  37 ('7') = */ 4.0,
    /*  38 ('8') = */ 4.0,
    /*  39 ('9') = */ 4.0,
    /*  3a (':') = */ 1.0,
    /*  3b (';') = */ 1.0,
    /*  3c ('<') = */ 3.0,
    /*  3d ('=') = */ 3.0,
    /*  3e ('>') = */ 3.0,
    /*  3f ('?') = */ 2.0,
    /*  40 ('@') = */ 3.0,
    /*  41 ('A') = */ 5.0,
    /*  42 ('B') = */ 5.0,
    /*  43 ('C') = */ 5.0,
    /*  44 ('D') = */ 5.0,
    /*  45 ('E') = */ 5.0,
    /*  46 ('F') = */ 5.0,
    /*  47 ('G') = */ 5.0,
    /*  48 ('H') = */ 5.0,
    /*  49 ('I') = */ 1.0,
    /*  4a ('J') = */ 5.0,
    /*  4b ('K') = */ 5.0,
    /*  4c ('L') = */ 5.0,
    /*  4d ('M') = */ 5.0,
    /*  4e ('N') = */ 5.0,
    /*  4f ('O') = */ 5.0,
    /*  50 ('P') = */ 5.0,
    /*  51 ('Q') = */ 5.0,
    /*  52 ('R') = */ 5.0,
    /*  53 ('S') = */ 5.0,
    /*  54 ('T') = */ 5.0,
    /*  55 ('U') = */ 5.0,
    /*  56 ('V') = */ 5.0,
    /*  57 ('W') = */ 5.0,
    /*  58 ('X') = */ 5.0,
    /*  59 ('Y') = */ 5.0,
    /*  5a ('Z') = */ 5.0,
    /*  5b ('[') = */ 2.0,
    /*  5c ('\') = */ 3.0,
    /*  5d (']') = */ 2.0,
    /*  5e ('^') = */ 1.0,
    /*  5f ('_') = */ 3.0,
    /*  60 ('`') = */ 1.0,
    /*  61 ('a') = */ 3.0,
    /*  62 ('b') = */ 3.0,
    /*  63 ('c') = */ 3.0,
    /*  64 ('d') = */ 3.0,
    /*  65 ('e') = */ 3.0,
    /*  66 ('f') = */ 3.0,
    /*  67 ('g') = */ 3.0,
    /*  68 ('h') = */ 3.0,
    /*  69 ('i') = */ 1.0,
    /*  6a ('j') = */ 2.0,
    /*  6b ('k') = */ 3.0,
    /*  6c ('l') = */ 1.0,
    /*  6d ('m') = */ 5.0,
    /*  6e ('n') = */ 3.0,
    /*  6f ('o') = */ 3.0,
    /*  70 ('p') = */ 3.0,
    /*  71 ('q') = */ 3.0,
    /*  72 ('r') = */ 2.0,
    /*  73 ('s') = */ 3.0,
    /*  74 ('t') = */ 2.0,
    /*  75 ('u') = */ 3.0,
    /*  76 ('v') = */ 3.0,
    /*  77 ('w') = */ 3.0,
    /*  78 ('x') = */ 3.0,
    /*  79 ('y') = */ 3.0,
    /*  7a ('z') = */ 3.0,
    /*  7b ('{') = */ 2.0,
    /*  7c ('|') = */ 1.0,
    /*  7d ('}') = */ 2.0,
    /*  7e ('~') = */ 2.0
};


/*
 *	What size is a string (in points)?
 */

#define FUDGE (2.834646 * 0.6)

floatish
StringSize(s)
  char* s;
{
    floatish r;

    for (r = 0.0; *s; s++) {
	r += fonttab[(*s) - 0x20];
    }

    return r * FUDGE;
}
