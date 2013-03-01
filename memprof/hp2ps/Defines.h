#ifndef DEFINES_H
#define DEFINES_H

/* 
 * Things that can be altered.
 */

#define THRESHOLD_PERCENT _thresh_ /* all values below 1% insignificant      */
#define DEFAULT_THRESHOLD      1.0
extern floatish _thresh_;

#define TWENTY 		  _twenty_ /* show top 20 bands, grouping excess     */
#define DEFAULT_TWENTY		20 /* this is default and absolute maximum   */
extern int _twenty_;

#define LARGE_FONT	        12  /* Helvetica 12pt 			     */
#define NORMAL_FONT		10  /* Helvetica 10pt 			     */

#define BORDER_HEIGHT        432.0  /* page border box 432pt (6 inches high) */
#define BORDER_WIDTH         648.0  /* page border box 648pt (9 inches wide) */
#define BORDER_SPACE	       5.0  /* page border space 		     */
#define BORDER_THICK           0.5  /* page border line thickness 0.5pt      */


#define TITLE_HEIGHT	      20.0  /* title box is 20pt high		     */
#define TITLE_TEXT_FONT LARGE_FONT  /* title in large font	             */
#define TITLE_TEXT_SPACE       6.0  /* space between title text and box      */


#define AXIS_THICK	       0.5  /* axis thickness 0.5pt                  */
#define AXIS_TEXT_SPACE	 	 6  /* space between axis legends and axis   */
#define AXIS_TEXT_FONT NORMAL_FONT  /* axis legends in normal font           */
#define AXIS_Y_TEXT_SPACE       35  /* space for y axis text                 */ 

#define KEY_BOX_WIDTH	        14  /* key boxes are 14pt high               */

#define SMALL_JOB_STRING_WIDTH	35  /* small title for 35 characters or less */
#define BIG_JOB_STRING_WIDTH    80  /* big title for everything else	     */	

#define GRAPH_X0	(AXIS_Y_TEXT_SPACE + (2 * BORDER_SPACE)) 
#define GRAPH_Y0	(AXIS_TEXT_FONT + (2 * BORDER_SPACE)) 


/*
 * Things that should be left well alone.
 */



#define START_X  72     /* start  72pt (1 inch)   from left   (portrait)  */
#define START_Y 108     /* start 108pt (1.5 inch) from bottom (portrait)  */

#define NUMBER_LENGTH            32

#define N_CHUNK			 24 

#define VERSION			"0.25"		/* as of 95/03/21	 */

#define max(x,y) ((x) > (y) ? (x) : (y))	/* not everyone has this */

#endif /* DEFINES_H */
