#ifndef _alloc_
#define _alloc_


#include "misc.h"
#include "mlvalues.h"

value alloc P((mlsize_t, tag_t));
value alloc_tuple P((mlsize_t));
value alloc_string P((mlsize_t));
value alloc_final P((mlsize_t, final_fun, mlsize_t, mlsize_t));
value copy_string P((char *));
value copy_string_array P((char **));
value copy_double P((double));
value alloc_array P((value (*funct) P((char *)), char ** array));
int convert_flag_list P((value, int *));


#endif /* _alloc_ */
