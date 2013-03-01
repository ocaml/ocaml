#include <stdio.h>
#include "Main.h"
#include "Defines.h"
#include "Error.h"
#include "HpFile.h"
#include "Utilities.h"

/* own stuff */
#include "AreaBelow.h"

extern void free();

/*
 *      Return the area enclosed by all of the curves. The algorithm
 *	used is the same as the trapizoidal rule for integration. 
 */
 
floatish
AreaBelow()
{
    intish i;
    intish j;
    intish bucket;
    floatish value;
    struct chunk *ch;
    floatish area;
    floatish trap;
    floatish base;
    floatish *maxima;

    maxima = (floatish *) xmalloc(nsamples * sizeof(floatish));
    for (i = 0; i < nsamples; i++) {
        maxima[i] = 0.0;
    }   

    for (i = 0; i < nidents; i++) {
        for (ch = identtable[i]->chk; ch; ch = ch->next) {
            for (j = 0; j < ch->nd; j++) {
                bucket = ch->d[j].bucket;
                value  = ch->d[j].value;
		if (bucket >= nsamples)
		    Disaster("bucket out of range");
                maxima[ bucket ] += value;
            }   
        }    
    }    

    area = 0.0;

    for (i = 1; i < nsamples; i++) {
	base = samplemap[i] - samplemap[i-1];
        if (maxima[i] > maxima[i-1]) {
	    trap = base * maxima[i-1] + ((base * (maxima[i] - maxima[i-1]))/ 2.0);
	} else {
	    trap = base * maxima[i]   + ((base * (maxima[i-1] - maxima[i]))/ 2.0);
        }

	area += trap;
    }

    free(maxima);
    return area;
}
