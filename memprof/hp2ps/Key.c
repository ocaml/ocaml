#include <stdio.h>
#include <math.h>
#include "Main.h"
#include "Defines.h"
#include "Dimensions.h"
#include "HpFile.h"
#include "Shade.h"

/* own stuff */
#include "Key.h"

static void KeyEntry PROTO((floatish, char *, floatish));

void Key()
{
    intish i;
    floatish c;
    floatish dc;

    for (i = 0; i < nidents; i++)    /* count identifiers */ 
	;

    c  = graphy0;
    dc = graphheight / (floatish) (i + 1);

    for (i = 0; i < nidents; i++) {
	c += dc;
	KeyEntry(c, identtable[i]->name, ShadeOf(identtable[i]->name));
    }
}



static void
KeyEntry(centreline, name, colour)
  floatish centreline; char* name; floatish colour;
{
    floatish namebase;
    floatish keyboxbase;
    floatish kstart;

    namebase = centreline - (floatish) (NORMAL_FONT / 2);
    keyboxbase = centreline - ((floatish) KEY_BOX_WIDTH / 2.0);

    kstart = graphx0 + graphwidth;

    fprintf(psfp, "%f %f moveto\n", kstart + borderspace, keyboxbase);
    fprintf(psfp, "0 %d rlineto\n", KEY_BOX_WIDTH);
    fprintf(psfp, "%d 0 rlineto\n", KEY_BOX_WIDTH);
    fprintf(psfp, "0 %d rlineto\n", -KEY_BOX_WIDTH);
    fprintf(psfp, "closepath\n");

    fprintf(psfp, "gsave\n"); 
    SetPSColour(colour);
    fprintf(psfp, "fill\n");
    fprintf(psfp, "grestore\n");
    fprintf(psfp, "stroke\n");

    fprintf(psfp, "HE%d setfont\n", NORMAL_FONT);
    fprintf(psfp, "%f %f moveto\n", kstart + (floatish) KEY_BOX_WIDTH + 2 * borderspace, namebase);

    fprintf(psfp, "(%s) show\n", name); 
}
