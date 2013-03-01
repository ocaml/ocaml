#include <stdio.h>
#include "Main.h"
#include "Defines.h"
#include "Dimensions.h"
#include "Error.h"
#include "HpFile.h"
#include "Utilities.h"

/* own stuff */
#include "Scale.h"

/*
 *	Return the maximum combined height that all the sample
 *	curves will reach. This (absolute) figure can then be 
 *	used to scale the samples automatically so that they
 *	fit on the page.
 */

extern void free();

floatish
MaxCombinedHeight()
{
    intish i;
    intish j;
    floatish mx;
    int bucket;
    floatish value;
    struct chunk* ch;
    floatish *maxima; 

    maxima = (floatish*) xmalloc(nsamples * sizeof(floatish));
    for (i = 0; i < nsamples; i++) {
        maxima[ i ] = 0.0;
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

    for (mx = maxima[ 0 ], i = 0; i < nsamples; i++) {
        if (maxima[ i ] > mx) mx = maxima[ i ];
    } 

    free(maxima);
    return mx;
}



/*
 *	Scale the values from the samples so that they will fit on 
 *	the page.	
 */

extern floatish xrange;
extern floatish yrange;

void
Scale()
{
    intish i;
    intish j;
    floatish sf;
    struct chunk* ch;

    if (yrange == 0.0)		/* no samples */
	return;

    sf = graphheight / yrange; 

    for (i = 0; i < nidents; i++) {
        for (ch = identtable[i]->chk; ch; ch = ch->next) {
            for (j = 0; j < ch->nd; j++) {
	        ch->d[j].value = ch->d[j].value * sf;
            }    
        }    
    }
}
