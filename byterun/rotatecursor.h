/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Damien Doligez, projet Para, INRIA Rocquencourt          */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

/* rotatecursor library, written by <Damien.Doligez@inria.fr>
   This file is in the public domain.

   version 1.13

   The goal of this library is to help implement cooperative multitasking
   for MPW tools: to make sure that your program calls RotateCursor often
   enough (about 20 times per second) but not too often (to avoid a big
   slowdown).
   It can also be used for applications with a little more work.


   Simple usage for MPW tools:
   ^^^^^^^^^^^^^^^^^^^^^^^^^^^
   1. #include this file where appropriate
   2. Insert the following line in every loop of your program:
        ROTATECURSOR_MAGIC ();
      The overhead of this macro is only a few CPU cycles, so it can be
      used without problem even in tight loops.


   Simple usage for applications:
   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
   1. Write a function with prototype:
        pascal void myaction (long x);
      This functions should get and handle all events until the event
      queue is empty (i.e. until it gets a null event).  It should also
      animate the cursor.
   2. #include this file where appropriate
   3. Add this line to the init code of your program:
        rotatecursor_options (NULL, 0, &myaction);
   4. Insert the following line in every loop of your program:
        ROTATECURSOR_MAGIC ();
      The overhead of this macro is only a few CPU cycles, so it can be
      used without problem even in tight loops.
   5. If there is no function called RotateCursor in your libraries, you
      will have to provide one that does nothing (it will not be called).

   See below for advanced options.
*/

#ifndef _rotatecursor_h_
#define _rotatecursor_h_

extern int volatile rotatecursor_flag;
/*
  [rotatecursor_flag] will be automagically set to 1 when the time comes
  to call [rotatecursor_ticker].
*/


void rotatecursor_options (int volatile *p1, int period,
                           pascal void (*action) (long));
/*
  Use [rotatecursor_options] to change advanced parameters:

  1. [p1] is a pointer to another variable that will be set to 1 when
     the time is up, unless it is already nonzero.  Typical use is when
     you already have a variable in your program that is set
     asynchronously for something else, and you want to avoid testing
     two different variables in your inner loop.  Pass [NULL] in this
     argument if you don't need this feature.

  2. [period] is the interval (in milliseconds) between calls to
     RotateCursor.  Reasonable values are between 10 and 200.
     If you pass 0 in this argument, the default value (50) will
     be used.  This value is passed to PrimeTime, so a negative value
     represents a delay in microseconds (not very useful here...)

  3. [action] is the function that will be called at regular intervals
     by [rotatecursor_ticker].  If you pass [NULL] in this argument,
     the default function, [RotateCursor], will be called.
*/

int rotatecursor_rearm (void);
/*
  [rotatecursor_rearm] resets [rotatecursor_flag] to 0 and rearms the
  Time Manager task that will set [rotatecursor_flag] to 1 after the
  appropriate delay.
  You can use [rotatecursor_rearm] if some part of your program needs
  to perform a periodic action that is not the normal one set up
  with [rotatecursor_options].
  This function always returns 0.
*/

int rotatecursor_ticker (void);
/*
  [rotatecursor_ticker] calls [rotatecursor_rearm] (see below) and your
  [action] function (or [RotateCursor]).
  This function always returns 0.  It returns an int so you can use
  it in an expression as well as a statement.
 */

#define ROTATECURSOR_MAGIC() (rotatecursor_flag ? rotatecursor_ticker () : 0)
/*
  [ROTATECURSOR_MAGIC] is a simple interface to [rotatecursor_flag]
  and [rotatecursor_ticker].  Can be used as a statement (followed by
  a semicolon) or in an expression (followed by a comma).
*/

void rotatecursor_final (void);
/*
  [rotatecursor_final] is set up (with [atexit]) to be called before your
  program finishes.  If for any reason the [atexit] functions are not
  called before your program exits, you should call this function by hand.
  It is harmless to call [rotatecursor_final] twice.
*/

#endif /* _rotatecursor_h_ */
