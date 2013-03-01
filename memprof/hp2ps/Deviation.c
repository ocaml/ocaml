#include <stdio.h>
#include <string.h>
#include <math.h>
#include "Main.h"
#include "Defines.h"
#include "Error.h"
#include "HpFile.h"
#include "Utilities.h"

extern void free();

/* own stuff */
#include "Deviation.h"

/*
 *	Reorder the identifiers in the identifier table so that the
 *	ones whose data points exhibit the mininal standard deviation
 *	come first.	
 */

void
Deviation()
{
    intish i;
    intish j;
    floatish dev;
    struct chunk* ch;
    int min;
    floatish t;
    struct entry* e;
    floatish *averages; 
    floatish *deviations;

    averages   = (floatish*) xmalloc(nidents * sizeof(floatish));
    deviations = (floatish*) xmalloc(nidents * sizeof(floatish));

    /* find averages */

    for (i = 0; i < nidents; i++) {
	averages[i] = 0.0;
    }
 
    for (i = 0; i < nidents; i++) {
        for (ch = identtable[i]->chk; ch; ch = ch->next) {
	    for (j = 0; j < ch->nd; j++) {
	        averages[i] += ch->d[j].value; 
	    }
        }
    }    

    for (i = 0; i < nidents; i++) {
        averages[i] /= (floatish) nsamples;
    }

    /* calculate standard deviation */

    for (i = 0; i < nidents; i++) {
	deviations[i] = 0.0;
    }
 
    for (i = 0; i < nidents; i++) {
        for (ch = identtable[i]->chk; ch; ch = ch->next) {
	    for (j = 0; j < ch->nd; j++) {
		dev = ch->d[j].value - averages[i];
	        deviations[i] += dev * dev; 
	    }
        }
    }

    for (i = 0; i < nidents; i++) {
        deviations[i] = (floatish) sqrt ((doublish) (deviations[i] / 
					 (floatish) (nsamples - 1)));
    }


    /* sort on basis of standard deviation */

    for (i = 0; i < nidents-1; i++) {
	min = i; 
	for (j = i+1; j < nidents; j++) {
	    if (deviations[ j ] < deviations[min]) {
		min = j;
	    }
	}

        t = deviations[min]; 
	deviations[min] = deviations[i];	
	deviations[i] = t;

        e = identtable[min];
	identtable[min] = identtable[i];
	identtable[i] = e;
    } 	

    free(averages);
    free(deviations);
}

void
Identorder(iflag)
    int iflag;	/* a funny three-way flag ? WDP 95/03 */
{
    int i;
    int j;
    int min;
    struct entry* e;

    /* sort on basis of ident string */
    if (iflag > 0) {
	/* greatest at top i.e. smallest at start */

	for (i = 0; i < nidents-1; i++) {
	    min = i; 
	    for (j = i+1; j < nidents; j++) {
		if (strcmp(identtable[j]->name, identtable[min]->name) < 0) {
		    min = j;
		}
	    }

	    e = identtable[min];
	    identtable[min] = identtable[i];
	    identtable[i] = e;
	} 	
    } else {
	/* smallest at top i.e. greatest at start */

	for (i = 0; i < nidents-1; i++) {
	    min = i; 
	    for (j = i+1; j < nidents; j++) {
		if (strcmp(identtable[j]->name, identtable[min]->name) > 0) {
		    min = j;
		}
	    }

	    e = identtable[min];
	    identtable[min] = identtable[i];
	    identtable[i] = e;
	} 	
    }	
}
