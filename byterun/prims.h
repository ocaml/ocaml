/* Interface with C primitives. */

#ifndef _prims_
#define _prims_

typedef value (*c_primitive)();

extern c_primitive cprim[];
extern char * names_of_cprim[];

#endif /* _prims_ */
