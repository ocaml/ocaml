/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*                          Sadiq Jaffer, Opsian                          */
/*                 Stephen Dolan, University of Cambridge                 */
/*                      Enguerrand Decorne, Tarides                       */
/*                                                                        */
/*   Copyright 2021 Opsian Ltd                                            */
/*   Copyright 2020 University of Cambridge                               */
/*   Copyright 2020 Tarides                                               */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

/* Runtime events - ring buffer-based runtime tracing

   This file contains the C API for runtime events. It is intended for use
   cases where the OCaml API is impractical. See the Runtime_events OCaml
   module source for more detailed information about the system and
   environment variables to control it externally.

*/

#ifndef CAML_RUNTIME_EVENTS_H
#define CAML_RUNTIME_EVENTS_H

#include "mlvalues.h"

#ifdef CAML_INSTR
#define CAML_EV_ALLOC(s) caml_ev_alloc(s)
#define CAML_EV_ALLOC_FLUSH() caml_ev_alloc_flush()
#else
#define CAML_EV_ALLOC(s)      /**/
#define CAML_EV_ALLOC_FLUSH() /**/
#endif

#define CAML_EV_BEGIN(p) caml_ev_begin(p)
#define CAML_EV_END(p) caml_ev_end(p)
#define CAML_EV_COUNTER(c,v) caml_ev_counter(c,v)
#define CAML_EV_LIFECYCLE(l,d) caml_ev_lifecycle(l,d)
#define CAML_RUNTIME_EVENTS_INIT() caml_runtime_events_init()
#define CAML_RUNTIME_EVENTS_DESTROY() caml_runtime_events_destroy()
typedef enum {
    EV_INTERNAL,
    EV_LIFECYCLE,
    EV_BEGIN,
    EV_EXIT,
    EV_COUNTER,
    EV_ALLOC,
    EV_FLUSH
} ev_runtime_message_type;

typedef enum {
    EV_GC
} ev_event_type;

/* See runtime_events.mli for event documentation */

typedef enum {
    EV_RING_START,
    EV_RING_STOP,
    EV_RING_PAUSE,
    EV_RING_RESUME,
    EV_FORK_PARENT,
    EV_FORK_CHILD,
    EV_DOMAIN_SPAWN,
    EV_DOMAIN_TERMINATE
} ev_lifecycle;

typedef enum {
    EV_EXPLICIT_GC_SET,
    EV_EXPLICIT_GC_STAT,
    EV_EXPLICIT_GC_MINOR,
    EV_EXPLICIT_GC_MAJOR,
    EV_EXPLICIT_GC_FULL_MAJOR,
    EV_EXPLICIT_GC_COMPACT,
    EV_MAJOR,
    EV_MAJOR_SWEEP,
    EV_MAJOR_MARK_ROOTS,
    EV_MAJOR_MEMPROF_ROOTS,
    EV_MAJOR_MARK,
    EV_MINOR,
    EV_MINOR_LOCAL_ROOTS,
    EV_MINOR_MEMPROF_ROOTS,
    EV_MINOR_MEMPROF_CLEAN,
    EV_MINOR_FINALIZED,
    EV_EXPLICIT_GC_MAJOR_SLICE,
    EV_FINALISE_UPDATE_FIRST,
    EV_FINALISE_UPDATE_LAST,
    EV_INTERRUPT_REMOTE,
    EV_MAJOR_EPHE_MARK,
    EV_MAJOR_EPHE_SWEEP,
    EV_MAJOR_FINISH_MARKING,
    EV_MAJOR_GC_CYCLE_DOMAINS,
    EV_MAJOR_GC_PHASE_CHANGE,
    EV_MAJOR_GC_STW,
    EV_MAJOR_MARK_OPPORTUNISTIC,
    EV_MAJOR_SLICE,
    EV_MAJOR_FINISH_CYCLE,
    EV_MINOR_CLEAR,
    EV_MINOR_FINALIZERS_OLDIFY,
    EV_MINOR_GLOBAL_ROOTS,
    EV_MINOR_LEAVE_BARRIER,
    EV_STW_API_BARRIER,
    EV_STW_HANDLER,
    EV_STW_LEADER,
    EV_MAJOR_FINISH_SWEEPING,
    EV_MAJOR_MEMPROF_CLEAN,
    EV_MINOR_FINALIZERS_ADMIN,
    EV_MINOR_REMEMBERED_SET,
    EV_MINOR_REMEMBERED_SET_PROMOTE,
    EV_MINOR_LOCAL_ROOTS_PROMOTE,
    EV_DOMAIN_CONDITION_WAIT,
    EV_DOMAIN_RESIZE_HEAP_RESERVATION,
    EV_COMPACT,
    EV_COMPACT_EVACUATE,
    EV_COMPACT_FORWARD,
    EV_COMPACT_RELEASE,
    EV_EMPTY_MINOR
} ev_runtime_phase;

typedef enum {
    EV_C_FORCE_MINOR_ALLOC_SMALL,
    EV_C_FORCE_MINOR_MAKE_VECT,
    EV_C_FORCE_MINOR_SET_MINOR_HEAP_SIZE,
    EV_C_FORCE_MINOR_MEMPROF,

    EV_C_MINOR_PROMOTED,
    EV_C_MINOR_ALLOCATED,

    EV_C_REQUEST_MAJOR_ALLOC_SHR,
    EV_C_REQUEST_MAJOR_ADJUST_GC_SPEED,
    EV_C_REQUEST_MINOR_REALLOC_REF_TABLE,
    EV_C_REQUEST_MINOR_REALLOC_EPHE_REF_TABLE,
    EV_C_REQUEST_MINOR_REALLOC_CUSTOM_TABLE,

    EV_C_MAJOR_HEAP_POOL_WORDS,
    EV_C_MAJOR_HEAP_POOL_LIVE_WORDS,
    EV_C_MAJOR_HEAP_LARGE_WORDS,
    EV_C_MAJOR_HEAP_POOL_FRAG_WORDS,
    EV_C_MAJOR_HEAP_POOL_LIVE_BLOCKS,
    EV_C_MAJOR_HEAP_LARGE_BLOCKS,

    EV_C_MAJOR_HEAP_WORDS,
    EV_C_MAJOR_ALLOCATED_WORDS,
    EV_C_MAJOR_ALLOCATED_WORK,
    EV_C_MAJOR_DEPENDENT_WORK,
    EV_C_MAJOR_EXTRA_WORK,
    EV_C_MAJOR_WORK_COUNTER,
    EV_C_MAJOR_ALLOC_COUNTER,
    EV_C_MAJOR_SLICE_TARGET,
    EV_C_MAJOR_SLICE_BUDGET
} ev_runtime_counter;

typedef enum {
    EV_USER_SPAN_BEGIN,
    EV_USER_SPAN_END
} ev_user_span;

/* external C-API for reading from the runtime_events */
struct caml_runtime_events_cursor;

typedef enum {
  E_SUCCESS = 0,
  E_CURSOR_NOT_OPEN = -1,
  E_CORRUPT_STREAM = -2,
  E_ALLOC_FAIL = -3,
  E_PATH_FAILURE = -4,
  E_OPEN_FAILURE = -5,
  E_NO_CURRENT_RING = -6,
  E_MAP_FAILURE = -7,
  E_CURSOR_POLL_BUSY = -8,
} runtime_events_error;

#ifdef __cplusplus
extern "C" {
#endif

/* Starts runtime_events. Needs to be called before
   [caml_runtime_events_create_cursor]. Needs the runtime lock held to call and
   will trigger a stop-the-world pause. */
CAMLextern void caml_runtime_events_start(void);

/* Pauses runtime_events if not currently paused otherwise does nothing.
   No new events (other than the pause itself) will be written to the ring
   buffer by this domain immediately and all other domains soon. Needs the
   runtime lock held to call as a pause event is written during this call. */
CAMLextern void caml_runtime_events_pause(void);

/* Resumes runtime_events if currently paused otherwise does nothing. New events
   (as well as a resume event) will be written to this domain immediately and
   all other domains soon. Needs the runtime lock held to call as a resume event
   is written during this call. */
CAMLextern void caml_runtime_events_resume(void);

/* Returns [1] if runtime events are currently active (started and not paused),
   [0] otherwise. */
CAMLextern int caml_runtime_events_are_active(void);

#ifdef __cplusplus
}
#endif

#ifdef CAML_INTERNALS

struct runtime_events_buffer_header {
  atomic_uint_fast64_t ring_head;
  atomic_uint_fast64_t ring_tail;
  uint64_t padding[8]; /* Padding so headers don't share cache lines. Eight
                          words guarantees that buffer headers don't share
                          cache lines, even for non-aligned allocations. */
};

#define RUNTIME_EVENTS_CUSTOM_EVENT_ID_LENGTH 128

struct runtime_events_custom_event {
   char name[RUNTIME_EVENTS_CUSTOM_EVENT_ID_LENGTH];
};

/* The type for event messages in the ring. Span is separated in two types as an
   optimization to avoid associating a value with the span event. */
typedef enum {
   EV_USER_MSG_TYPE_UNIT,
   EV_USER_MSG_TYPE_INT,
   EV_USER_MSG_TYPE_SPAN_BEGIN,
   EV_USER_MSG_TYPE_SPAN_END,
   EV_USER_MSG_TYPE_CUSTOM
} ev_user_message_type;

typedef union {
   ev_runtime_message_type runtime;
   ev_user_message_type user;
} ev_message_type;

/* The type for event messages in OCaml. */
typedef enum {
   EV_USER_ML_TYPE_UNIT,
   EV_USER_ML_TYPE_INT,
   EV_USER_ML_TYPE_SPAN,
   EV_USER_ML_TYPE_CUSTOM
} ev_user_ml_type;

/* For a more detailed explanation of the runtime_events file layout, see
   runtime_events.c */
struct runtime_events_metadata_header {
  uint64_t version;
  uint64_t max_domains;
  uint64_t ring_header_size_bytes; /* Ring buffer header size (bytes) */
  uint64_t ring_size_bytes; /* Ring data size (bytes) */
  uint64_t ring_size_elements; /* Ring size in 64-bit elements */
  uint64_t headers_offset; /* Offset from this struct to first header (bytes) */
  uint64_t data_offset; /* Offset from this struct to first data (byte) */
  uint64_t custom_events_offset; /* Offset from this struct to first custom
                                    event (byte) */
};

#define RUNTIME_EVENTS_MAX_CUSTOM_EVENTS (1 << 13)
#define RUNTIME_EVENTS_MAX_MSG_LENGTH (1 << 10)

/* Number of tens of single-size buckets */
#define RUNTIME_EVENTS_NUM_ALLOC_BUCKETS_SINGLE 1
/* Number of buckets of 10 sizes */
#define RUNTIME_EVENTS_NUM_ALLOC_BUCKETS_DECADE 9
/* Total number of buckets */
#define RUNTIME_EVENTS_NUM_ALLOC_BUCKETS \
  (10 * RUNTIME_EVENTS_NUM_ALLOC_BUCKETS_SINGLE \
   + RUNTIME_EVENTS_NUM_ALLOC_BUCKETS_DECADE \
   + 1)

/* event header fields (for runtime events):
- length (10 bits)
- runtime or user event (1 bit)
- event type (4 bits)
- event id (13 bits)
*/

#define RUNTIME_EVENTS_ITEM_LENGTH(header) \
        (((header) >> 54) & ((1UL << 10) - 1))
#define RUNTIME_EVENTS_ITEM_IS_RUNTIME(header) !((header) & (1ULL << 53))
#define RUNTIME_EVENTS_ITEM_IS_USER(header) ((header) & (1ULL << 53))
#define RUNTIME_EVENTS_ITEM_TYPE(header) (((header) >> 49) & ((1UL << 4) - 1))
#define RUNTIME_EVENTS_ITEM_ID(header) (((header) >> 36) & ((1UL << 13) - 1))

#define RUNTIME_EVENTS_HEADER(length, is_runtime, type, event_id) \
         (((uint64_t)(length)) << 54) | \
         ((is_runtime) ? 0 : (1ULL << 53)) | \
         ((uint64_t)(type)) << 49 | \
         ((uint64_t)(event_id)) << 36;

/* Set up runtime_events (and check if we need to start it immediately).
   Called from startup* */
void caml_runtime_events_init(void);

/* Destroy all allocated runtime_events structures and clear up the ring.
   Called from [caml_sys_exit] */
void caml_runtime_events_destroy(void);

/* Handle safely re-initialising the runtime_events structures
   in a forked child */
CAMLextern void caml_runtime_events_post_fork(void);

/* Return the path of the ring buffers file of this process, or NULL
   if runtime events are not enabled. This is used in the consumer to
   read the ring buffers of the current process. Always returns a
   freshly-allocated string. */
CAMLextern char_os* caml_runtime_events_current_location(void);

/* Functions for putting runtime data on to the runtime_events. These are all
   internal to the runtime, except for caml_ev_lifecycle which is needed in
   otherlibs/unix/fork.c so must be declared CAMLextern in order to work on
   Cygwin. */
void caml_ev_begin(ev_runtime_phase phase);
void caml_ev_end(ev_runtime_phase phase);
void caml_ev_counter(ev_runtime_counter counter, uint64_t val);
CAMLextern void caml_ev_lifecycle(ev_lifecycle lifecycle, int64_t data);

/* caml_ev_alloc records the (bucketed) size of allocations into the major heap.
   It appears only in alloc_shr and caml_shared_try_alloc. These buckets are
   meant to be flushed explicitly by the caller through the caml_ev_alloc_flush
   function. Until then the buckets are just updated until flushed.
*/
void caml_ev_alloc(uint64_t sz);
void caml_ev_alloc_flush(void);


/* Allocate a unique ID for the event and construct its value: there are at
   most RUNTIME_EVENTS_MAX_CUSTOM_EVENTS of them. */
CAMLextern value caml_runtime_events_user_register(value event_name,
   value event_tag, value event_type);

/* Write event data to ring buffer. Should not be called when the runtime lock
   is not held, i.e., after [caml_enter_blocking_section()] and before
   [caml_leave_blocking_section()]. */
CAMLextern value caml_runtime_events_user_write(
   value buf,
   value event,
   value event_content);

/* Resolve an event name to the associated event value using known registered
   events. */
CAMLextern value caml_runtime_events_user_resolve(char* event_name,
   ev_user_ml_type event_type);

#endif /* CAML_INTERNALS */

#endif /* CAML_RUNTIME_EVENTS_H */
