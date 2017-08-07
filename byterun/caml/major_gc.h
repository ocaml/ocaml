#ifndef CAML_MAJOR_GC
#define CAML_MAJOR_GC

intnat caml_major_collection_slice (intnat);
void caml_finish_marking (void);
void caml_init_major_gc(void);
void caml_teardown_major_gc(void);
void caml_darken(void*, value, value* ignored);
void caml_mark_root(value, value*);
void caml_empty_mark_stack(void);
void caml_finish_major_cycle(void);


struct heap_stats {
  intnat pool_words;
  intnat pool_max_words;
  intnat pool_live_words;
  intnat pool_live_blocks;
  intnat pool_frag_words;
  intnat large_words;
  intnat large_max_words;
  intnat large_blocks;
};
void caml_accum_heap_stats(struct heap_stats* acc, const struct heap_stats* s);

struct gc_stats {
  uint64 minor_words;
  uint64 promoted_words;
  uint64 major_words;
  uint64 minor_collections;
  struct heap_stats major_heap;
};
void caml_sample_gc_stats(struct gc_stats* buf);


#endif /* CAML_MAJOR_GC_H */
