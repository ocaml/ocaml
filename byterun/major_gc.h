#ifndef CAML_MAJOR_GC
#define CAML_MAJOR_GC

extern __thread value* caml_mark_stack;
extern __thread int caml_mark_stack_count;



intnat caml_major_collection_slice (intnat);
void caml_finish_marking (void);
void caml_init_major_gc(void);
void caml_darken(value, value* ignored);
void caml_mark_root(value, value*);
void caml_empty_mark_stack(void);

#endif
