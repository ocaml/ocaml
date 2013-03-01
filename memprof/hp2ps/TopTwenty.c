#include <stdio.h>
#include "Main.h"
#include "Defines.h"
#include "Error.h"
#include "HpFile.h"
#include "Utilities.h"

/* own stuff */
#include "TopTwenty.h"

/*
 *	We only have room in the key for a maximum of 20 identifiers. 
 *	We therefore choose to keep the top 20 bands --- these will 
 *	be the most important ones, since this pass is performed after 
 *	the threshold and standard deviation passes. If there are more 
 *	than 20 bands, the excess are gathered together as an "OTHER" ]
 *	band which appears as band 20.
 */

extern void free();

void
TopTwenty()
{
    intish i;
    intish j;
    intish compact;
    intish bucket;
    floatish value;
    struct entry* en;
    struct chunk* ch;
    floatish *other; 

    i = nidents;
    if (i <= TWENTY) return;	/* nothing to do! */

    other = (floatish*) xmalloc(nsamples * sizeof(floatish));
    /* build a list of samples for "OTHER" */ 

    compact = (i - TWENTY) + 1;

    for (i = 0; i < nsamples; i++) {
        other[ i ] = 0.0;
    }   

    for (i = 0; i < compact && i < nidents; i++) {
        for (ch = identtable[i]->chk; ch; ch = ch->next) {
            for (j = 0; j < ch->nd; j++) {
                bucket = ch->d[j].bucket;
                value  = ch->d[j].value;
		if (bucket >= nsamples)
		    Disaster("bucket out of range");
                other[ bucket ] += value;
            }   
        }    
    }    

    en = MakeEntry("OTHER");
    en->next = 0;

    for (i = 0; i < nsamples; i++) {
    	StoreSample(en, i, other[i]);
    }

    /* slide samples down */
    for (i = compact; i < nidents; i++) {
        identtable[i-compact+1] = identtable[i];
    }

    nidents = TWENTY;
    identtable[0] = en;
    free(other);
}
