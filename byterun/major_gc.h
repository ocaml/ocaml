/* major GC */

intnat caml_major_collection_slice (intnat);
void caml_finish_marking (void);
void caml_init_major_gc(void);
void caml_darken(value);
