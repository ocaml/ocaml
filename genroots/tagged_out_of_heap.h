/* SPDX-License-Identifier: MIT */
#ifndef ABSTRACT_OUT_OF_HEAP_H

#include "caml/mlvalues.h"

#define Block_data(b) ((value *)(b & ~((value)1)))

inline value alloc_tagged_block(void)
{
  value *b = malloc(sizeof(value));
  return (value)b | (value)1;
}

inline void free_tagged_block(value b)
{
  free(Block_data(b));
}

#endif // ABSTRACT_OUT_OF_HEAP_H
