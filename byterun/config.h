#ifndef _config_
#define _config_


#include "../config/m.h"
#include "../config/s.h"

/* Library dependencies */

#ifdef HAS_MEMMOVE
#define bcopy(src,dst,len) memmove((dst), (src), (len))
#else
#ifdef HAS_BCOPY
/* Nothing to do */
#else
#ifdef HAS_MEMCPY
#define bcopy(src,dst,len) memcpy((dst), (src), (len))
#else
#define bcopy(src,dst,len) memmov((dst), (src), (len))
#define USING_MEMMOV
#endif
#endif
#endif

#ifndef HAS__SETJMP
#define _setjmp setjmp
#define _longjmp longjmp
#endif

/* We use threaded code interpretation if the compiler provides labels
   as first-class values (GCC 2.x). */

#if defined(__GNUC__) && __GNUC__ >= 2 && !defined(DEBUG)
#define THREADED_CODE
#endif

/* Signed char type */

#if defined(__STDC__) || defined(SIGNED_CHAR_WORKS)
typedef signed char schar;
#else
typedef char schar;
#endif

/* Do not change this definition. */
#define Page_size (1 << Page_log)

/* Memory model parameters */

/* The size of a page for memory management (in bytes) is [1 << Page_log].
   It must be a multiple of [sizeof (long)]. */
#define Page_log 12             /* A page is 4 kilobytes. */

/* Initial size of stack (bytes). */
#define Stack_size 16384

/* Minimum free size of stack (bytes); below that, it is reallocated. */
#define Stack_threshold 1024

/* Maximum sizes for the stack (bytes). */
   
#define Max_stack_size 524288


/* Maximum size of a block allocated in the young generation (words). */
/* Must be > 4 */
#define Max_young_wosize 256


/* Minimum size of the minor zone (words).
   This must be at least [Max_young_wosize + 1]. */
#define Minor_heap_min 4096

/* Maximum size of the minor zone (words).
   Must be greater than or equal to [Minor_heap_min].
*/
#define Minor_heap_max (1 << 28)

/* Default size of the minor zone. (words)  */
#define Minor_heap_def 32768


/* Minimum size increment when growing the heap (words).
   Must be a multiple of [Page_size / sizeof (value)]. */
#define Heap_chunk_min (2 * Page_size / sizeof (value))

/* Maximum size of a contiguous piece of the heap (words).
   Must be greater than or equal to [Heap_chunk_min].
   Must be greater than or equal to [Bhsize_wosize (Max_wosize)]. */
#define Heap_chunk_max (Bhsize_wosize (Max_wosize))

/* Default size increment when growing the heap. (bytes)
   Must be a multiple of [Page_size / sizeof (value)]. */
#define Heap_chunk_def (62 * Page_size / sizeof (value))


/* Default speed setting for the major GC.  The heap will grow until
   the dead objects and the free list represent this percentage of the
   heap size.  The rest of the heap is live objects. */
#define Percent_free_def 30


#endif /* _config_ */
