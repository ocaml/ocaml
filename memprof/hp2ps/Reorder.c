#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "Main.h"
#include "Defines.h"
#include "Error.h"
#include "HpFile.h"
#include "Utilities.h"

/* own stuff */
#include "Reorder.h"

static struct order {
    char* ident;
    int order;
} *ordermap = 0;

static int ordermapmax = 0;
static int ordermapindex = 0;


void
OrderFor(ident, order)
  char* ident; 
  int order;
{
    if (! ordermap) {
	ordermapmax = (nidents > TWENTY ? nidents : TWENTY) * 2;
	         /* Assume nidents read is indication of the No of
		    idents in the .aux file (*2 for good luck !) */
	ordermap = xmalloc(ordermapmax * sizeof(struct order));
    }

    if (ordermapindex < ordermapmax) {
	ordermap[ ordermapindex ].ident = copystring(ident);
	ordermap[ ordermapindex ].order = order;
	ordermapindex++;
    } else {
	Disaster("order map overflow");
    }
}

/*
 *	Get the order of to be used for "ident" if there is one. 
 *	Otherwise, return 0 which is the minimum ordering value. 
 */

int
OrderOf(ident)
  char* ident;
{
    int i;

    for (i = 0; i < ordermapindex; i++) {
	if (strcmp(ordermap[i].ident, ident) == 0) {	/* got it */
	    return(ordermap[i].order);
	}
    }

    return 0; 
}

/*
 *	Reorder on the basis of information from ".aux" file.
 */

void
Reorder()
{
    intish i;
    intish j;
    int min;
    struct entry* e;
    int o1, o2;

    for (i = 0; i < nidents-1; i++) {
	min = i; 
	for (j = i+1; j < nidents; j++) {
	    o1 = OrderOf(identtable[  j  ]->name);
	    o2 = OrderOf(identtable[ min ]->name);

	    if (o1 < o2 ) min = j;
	}

        e = identtable[ min ];
	identtable[ min ] = identtable[ i ];
	identtable[ i ] = e;
    } 	
}
