/*
	WASTE Demo Project:
	Macintosh Controls with Long Values

	Copyright © 1993-1996 Marco Piovanelli
	All Rights Reserved

	C port by John C. Daub
*/

/***************************************************************************
   This file is not subject to the O'Caml licence.
   It is a slightly modified version of "LongControls.c" from
   the WASTE Demo source (version 1.2).
 ***************************************************************************/
/* $Id$ */

#ifndef __CONTROLS__
#include <Controls.h>
#endif

#ifndef __FIXMATH__
#include <FixMath.h>
#endif

#ifndef __TOOLUTILS__
#include <ToolUtils.h>
#endif

#include "main.h"                              /* The change */
#define BSL(A, B)	(((long) (A)) << (B))      /* is here    */


// long control auxiliary record used for keeping long settings
// a handle to this record is stored in the reference field of the control record

struct LCAuxRec
{
	long	value;	// long value
	long	min;	// long min
	long	max;	// long max
};
typedef struct LCAuxRec LCAuxRec, *LCAuxPtr, **LCAuxHandle;


OSErr LCAttach( ControlRef control )
{
	Handle		aux;
	LCAuxPtr	pAux;

	/*	allocate the auxiliary record that will hold long settings */

	if ( ( aux = NewHandleClear( sizeof( LCAuxRec ) ) ) == nil )
	{
		return	MemError( );
	}

	/*	store a handle to the auxiliary record in the contrlRfCon field */

	SetControlReference( control, (long) aux );

	/*	copy current control settings into the auxiliary record */

	pAux = * (LCAuxHandle) aux;
	pAux->value = GetControlValue( control );
	pAux->min = GetControlMinimum( control );
	pAux->max = GetControlMaximum( control );

	return noErr;
}

void LCDetach( ControlRef control )
{
	Handle aux;

	if ( ( aux = (Handle) GetControlReference( control ) ) != nil )
	{
		SetControlReference( control, 0L );
		DisposeHandle( aux );
	}
}

void LCSetValue( ControlRef control, long value )
{
	LCAuxPtr pAux;
	short controlMin, controlMax, newControlValue;

	pAux = * (LCAuxHandle) GetControlReference( control );

	/*	make sure value is in the range min...max */

	if ( value < pAux->min )
	{
		value = pAux->min;
	}
	if ( value > pAux->max )
	{
		value = pAux->max;
	}

	/*	save value in auxiliary record */

	pAux->value = value;

	/*	calculate new thumb position */

	controlMin = GetControlMinimum( control );
	controlMax = GetControlMaximum( control );
	newControlValue = controlMin + FixRound( FixMul ( FixDiv( value - pAux->min,
				pAux->max - pAux->min), BSL(controlMax - controlMin, 16 )));

	/*	do nothing if the thumb position hasn't changed */

	if ( newControlValue != GetControlValue(control) )
	{
		SetControlValue( control, newControlValue );
	}
}

void LCSetMin( ControlRef control, long min )
{
	LCAuxPtr pAux;

	pAux = * (LCAuxHandle) GetControlReference( control );

	/*	make sure min is less than or equal to max */

	if ( min > pAux->max )
	{
		min = pAux->max;
	}

	/*	save min in auxiliary record */

	pAux->min = min;

	/*	set control minimum to min or SHRT_MIN, whichever is greater */

	SetControlMinimum( control, ( min >= SHRT_MIN ) ? min : SHRT_MIN );

	/*	reset value */

	LCSetValue( control, pAux->value );
}

void LCSetMax( ControlRef control, long max )
{
	LCAuxPtr pAux;

	pAux = * (LCAuxHandle) GetControlReference( control );

	/*	make sure max is greater than or equal to min */

	if ( max < pAux->min )
	{
		max = pAux->min;
	}

	/*	save max in auxiliary record */

	pAux->max = max;

	/*	set control maximum to max or SHRT_MAX, whichever is less */

	SetControlMaximum( control, ( max <= SHRT_MAX ) ? max : SHRT_MAX );

	/*	reset value */

	LCSetValue( control, pAux->value );
}

/*	In each of these LCGetXXX() functions, there are 2 ways listed to do things.  They are
	both the same thing and perform the same stuff, just one is easier to read than the
	other (IMHO).  I asked Marco about it and he gave me the shorter code (what's commented
	in each function) and gave me this explanation:

		This version [the commented code] yields smaller and faster code
		(try disassembling both versions if you wish), but some people may
		find it somewhat harder to read.

	I agree with Marco that his code is better overall, but in the interest of readabilty
	(since this demo is a learning tool), I left my code in and put Marco's in commented
	out.  Pick whichever you'd like to use.
*/

long LCGetValue( ControlRef control )
{
	LCAuxPtr	pAux;

	pAux = *((LCAuxHandle)GetControlReference( control ));

	return pAux->value;

//	this is Marco's code.  Remember, this is a little harder to read, but overall
//	yields tighter code.

//	return (* (LCAuxHandle) GetControlReference(control)) -> value;

}

long LCGetMin( ControlRef control )
{
	LCAuxPtr	pAux;

	pAux = *((LCAuxHandle)GetControlReference( control ));

	return pAux->min;

//	this is Marco's code.  Remember, this is a little harder to read, but overall
//	yields tighter code.

//	return (* (LCAuxHandle)GetControlReference(control)) -> min;

}

long LCGetMax( ControlRef control )
{
	LCAuxPtr	pAux;

	pAux = *((LCAuxHandle)GetControlReference( control ));

	return pAux->max;

//	this is Marco's code.  Remember, this is a little harder to read, but overall
//	yields tighter code.

//	return (* (LCAuxHandle)GetControlReference(control)) -> max;

}

void LCSynch( ControlRef control )
{
	LCAuxPtr pAux;
	short controlMin, controlMax, controlValue;

	controlMin = GetControlMinimum( control );
	controlMax = GetControlMaximum( control );
	controlValue = GetControlValue( control );
	pAux = * (LCAuxHandle) GetControlReference( control );

	/*	calculate new long value */

	pAux->value = pAux->min + FixMul( FixRatio ( controlValue - controlMin,
				  controlMax - controlMin), pAux->max - pAux->min );
}

