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
#include <stdint.h>

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
} ev_message_type;

typedef enum {
    EV_GC
} ev_event_type;

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
    EV_MAJOR_MARK,
    EV_MINOR,
    EV_MINOR_LOCAL_ROOTS,
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
    EV_MINOR_FINALIZERS_ADMIN,
    EV_MINOR_REMEMBERED_SET,
    EV_MINOR_REMEMBERED_SET_PROMOTE,
    EV_MINOR_LOCAL_ROOTS_PROMOTE,
    EV_DOMAIN_CONDITION_WAIT,
    EV_DOMAIN_RESIZE_HEAP_RESERVATION
} ev_runtime_phase;

typedef enum {
    EV_C_ALLOC_JUMP,
    EV_C_FORCE_MINOR_ALLOC_SMALL,
    EV_C_FORCE_MINOR_MAKE_VECT,
    EV_C_FORCE_MINOR_SET_MINOR_HEAP_SIZE,
    EV_C_FORCE_MINOR_WEAK,
    EV_C_FORCE_MINOR_MEMPROF,
    EV_C_MAJOR_MARK_SLICE_REMAIN,
    EV_C_MAJOR_MARK_SLICE_FIELDS,
    EV_C_MAJOR_MARK_SLICE_POINTERS,
    EV_C_MAJOR_WORK_EXTRA,
    EV_C_MAJOR_WORK_MARK,
    EV_C_MAJOR_WORK_SWEEP,
    EV_C_MINOR_PROMOTED,
    EV_C_MINOR_ALLOCATED,
    EV_C_REQUEST_MAJOR_ALLOC_SHR,
    EV_C_REQUEST_MAJOR_ADJUST_GC_SPEED,
    EV_C_REQUEST_MINOR_REALLOC_REF_TABLE,
    EV_C_REQUEST_MINOR_REALLOC_EPHE_REF_TABLE,
    EV_C_REQUEST_MINOR_REALLOC_CUSTOM_TABLE
} ev_runtime_counter;

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

/* Starts runtime_events. Needs to be called before
   [caml_runtime_events_create_cursor]. Needs the runtime lock held to call and
   will trigger a stop-the-world pause. Returns Val_unit. */
CAMLextern value caml_runtime_events_start();

/* Pauses runtime_events if not currently paused otherwise does nothing.
   No new events (other than the pause itself) will be written to the ring
   buffer by this domain immediately and all other domains soon. Needs the
   runtime lock held to call as a pause event is written during this call.
   Returns Val_unit. */
CAMLextern value caml_runtime_events_pause();

/* Resumes runtime_events if currently paused otherwise does nothing. New events
   (as well as a resume event) will be written to this domain immediately and
   all other domains soon. Needs the runtime lock held to call as a resume event
   is written during this call. Returns Val_unit. */
CAMLextern value caml_runtime_events_resume();

#ifdef CAML_INTERNALS

struct runtime_events_buffer_header {
  atomic_uint_fast64_t ring_head;
  atomic_uint_fast64_t ring_tail;
  uint64_t padding[8]; /* Padding so headers don't share cache lines. Eight
                          words guarantees that buffer headers don't share
                          cache lines, even for non-aligned allocations. */
};

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
  uint64_t padding; /* Make the header a multiple of 64 bytes */
};

#define RUNTIME_EVENTS_NUM_ALLOC_BUCKETS 20
#define RUNTIME_EVENTS_MAX_MSG_LENGTH (1 << 10)

/* event header fields (for runtime events):
- length (10 bits)
- runtime or user event (1 bit)
- event type (4 bits)
- event id (13 bits)
*/

#define RUNTIME_EVENTS_ITEM_LENGTH(header) \
        (((header) >> 54) & ((1UL << 10) - 1))
#define RUNTIME_EVENTS_ITEM_IS_RUNTIME(header) !((header) | (1UL << 53))
#define RUNTIME_EVENTS_ITEM_IS_USER(header) ((header) | (1UL << 53))
#define RUNTIME_EVENTS_ITEM_TYPE(header) (((header) >> 49) & ((1UL << 4) - 1))
#define RUNTIME_EVENTS_ITEM_ID(header) (((header) >> 36) & ((1UL << 13) - 1))

#define RUNTIME_EVENTS_HEADER(length, is_runtime, type, event_id) \
         (((uint64_t)(length)) << 54) | \
         ((is_runtime) ? 0 : (1ULL << 53)) | \
         ((uint64_t)(type)) << 49 | \
         ((uint64_t)(event_id)) << 36;

/* Set up runtime_events (and check if we need to start it immediately).
   Called from startup* */
void caml_runtime_events_init();

/* Destroy all allocated runtime_events structures and clear up the ring.
   Called from [caml_sys_exit] */
void caml_runtime_events_destroy();

/* Handle safely re-initialising the runtime_events structures
   in a forked child */
void caml_runtime_events_post_fork();

/* Returns the location of the runtime_events for the current process if started
   or NULL otherwise */
CAMLextern char_os* caml_runtime_events_current_location();

/* Functions for putting runtime data on to the runtime_events */
void caml_ev_begin(ev_runtime_phase phase);
void caml_ev_end(ev_runtime_phase phase);
void caml_ev_counter(ev_runtime_counter counter, uint64_t val);
void caml_ev_lifecycle(ev_lifecycle lifecycle, int64_t data);

/* caml_ev_alloc records the (bucketed) size of allocations into the major heap.
   It appears only in alloc_shr and caml_shared_try_alloc. These buckets are
   meant to be flushed explicitly by the caller through the caml_ev_alloc_flush
   function. Until then the buckets are just updated until flushed.
*/
void caml_ev_alloc(uint64_t sz);
void caml_ev_alloc_flush();

#endif /* CAML_INTERNALS */

#endif /*CAML_RUNTIME_EVENTS_H*/
