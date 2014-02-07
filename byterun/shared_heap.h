

value* caml_shared_try_alloc(mlsize_t wosize, tag_t tag);
void caml_init_shared_heap();

int caml_mark_object(value);

int caml_sweep(int);


/* must be called during STW */
void caml_cycle_heap(void);
