#ifndef DIMENSIONS_H
#define DIMENSIONS_H

extern floatish borderheight; 
extern floatish borderwidth; 
extern floatish borderspace;
extern floatish borderthick;

extern floatish titleheight;
extern floatish titlewidth;
extern floatish titletextspace;

extern floatish graphx0;
extern floatish graphy0;

extern floatish graphheight;
extern floatish graphwidth;

void     Dimensions PROTO((void));
floatish StringSize PROTO((char *));

#endif /* DIMENSIONS_H */
