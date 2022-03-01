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

#ifndef CAML_EVENTRING_H
#define CAML_EVENTRING_H

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
#define CAML_EVENTRING_INIT() caml_eventring_init()
#define CAML_EVENTRING_DESTROY() caml_eventring_destroy()

#define CAML_EV_FLUSH() caml_ev_flush()

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
    EV_COMPACT_MAIN,
    EV_COMPACT_RECOMPACT,
    EV_EXPLICIT_GC_SET,
    EV_EXPLICIT_GC_STAT,
    EV_EXPLICIT_GC_MINOR,
    EV_EXPLICIT_GC_MAJOR,
    EV_EXPLICIT_GC_FULL_MAJOR,
    EV_EXPLICIT_GC_COMPACT,
    EV_MAJOR,
    EV_MAJOR_ROOTS,
    EV_MAJOR_SWEEP,
    EV_MAJOR_MARK_ROOTS,
    EV_MAJOR_MARK_MAIN,
    EV_MAJOR_MARK_FINAL,
    EV_MAJOR_MARK,
    EV_MAJOR_MARK_GLOBAL_ROOTS_SLICE,
    EV_MAJOR_ROOTS_GLOBAL,
    EV_MAJOR_ROOTS_DYNAMIC_GLOBAL,
    EV_MAJOR_ROOTS_LOCAL,
    EV_MAJOR_ROOTS_C,
    EV_MAJOR_ROOTS_FINALISED,
    EV_MAJOR_ROOTS_MEMPROF,
    EV_MAJOR_ROOTS_HOOK,
    EV_MAJOR_CHECK_AND_COMPACT,
    EV_MINOR,
    EV_MINOR_LOCAL_ROOTS,
    EV_MINOR_REF_TABLES,
    EV_MINOR_COPY,
    EV_MINOR_UPDATE_WEAK,
    EV_MINOR_FINALIZED,
    EV_EXPLICIT_GC_MAJOR_SLICE,
    EV_DOMAIN_SEND_INTERRUPT,
    EV_DOMAIN_IDLE_WAIT,
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

/* external C-API for reading from the eventring */
struct caml_eventring_cursor;

typedef enum {
  E_SUCCESS = 0,
  E_CURSOR_NOT_OPEN = -1,
  E_CORRUPT_STREAM = -2,
  E_ALLOC_FAIL = -3,
  E_PATH_FAILURE = -4,
  E_OPEN_FAILURE = -5,
  E_NO_CURRENT_RING = -6,
  E_MAP_FAILURE = -7,
} eventring_error;

/* Starts eventring. Needs to be called before [caml_eventring_create_cursor] */
extern value caml_eventring_start();

/* Pauses eventring. No new events (other than the pause itself) will be written
   to the eventrings by this domain immediately and all other domains soon. */
extern value caml_eventring_pause();

/* Removes eventring. New events (as well as a resume event) will be written to
   this domain immediately and all other domains soon. */
extern value caml_eventring_resume();

/* Create a cursor to read events from an eventring. Cursors can be created for
   eventrings in and out of process. To create one for the current process, pass
   [eventring_path] as NULL and a [pid] < 0. Otherwise [eventring_path] is a
   path to a directory containing the .eventring files. [pid] is the process id
   (or equivalent) of the startup OCaml process. The resulting cursor can be
   used with `caml_eventring_read_poll` to read events from the eventrings. */
extern eventring_error
caml_eventring_create_cursor(const char_os* eventring_path, int pid,
                             struct caml_eventring_cursor **cursor_res);

/* Set the runtime_begin event callback on the cursor */
extern void caml_eventring_set_runtime_begin(
    struct caml_eventring_cursor *cursor,
    int (*f)(int domain_id, void *callback_data, uint64_t timestamp,
             ev_runtime_phase phase));

/* Set the runtime_end event callback on the cursor */
extern void caml_eventring_set_runtime_end(
    struct caml_eventring_cursor *cursor,
    int (*f)(int domain_id, void *callback_data, uint64_t timestamp,
             ev_runtime_phase phase));

/* Set the runtime_counter event callback on the cursor */
extern void caml_eventring_set_runtime_counter(
    struct caml_eventring_cursor *cursor,
    int (*f)(int domain_id, void *callback_data, uint64_t timestamp,
             ev_runtime_counter counter, uint64_t val));

/* Set the alloc event callback on the cursor */
extern void
caml_eventring_set_runtime_alloc(struct caml_eventring_cursor *cursor,
                                 int (*f)(int domain_id, void *callback_data,
                                          uint64_t timestamp, uint64_t *sz));

/* Set the lifecycle event callback on the cursor */
extern void caml_eventring_set_lifecycle(
    struct caml_eventring_cursor *cursor,
    int (*f)(int domain_id, void *callback_data, int64_t timestamp,
              ev_lifecycle lifecycle, int64_t data));

/* Set the lost events callback on the cursor */
extern void caml_eventring_set_lost_events(
    struct caml_eventring_cursor *cursor,
    int (*f)(int domain_id, void *callback_data, int lost_words));

/* frees a cursor obtained from caml_eventring_creator_cursor */
extern void
caml_eventring_free_cursor(struct caml_eventring_cursor *cursor);

/* polls the eventring pointed to by [cursor] and calls the appropriate callback
    for each new event up to at most [max_events] times.

    Returns the number of events consumed in [events_consumed], if set.

    0 for [max_events] indicates no limit to the number of callbacks. */
CAMLextern eventring_error caml_eventring_read_poll(
    struct caml_eventring_cursor *cursor,
    void *callback_data,
    uintnat max_events, uintnat *events_consumed);

/* OCaml API for reading from the eventring. Documented in eventring.ml */
extern value caml_eventring_create_cursor_ml(value path_pid);
extern value caml_eventring_free_cursor_ml(value wrapped_cursor);
extern value caml_eventring_read_poll_ml(value wrapped_cursor,
                                              value callbacks,
                                              value max_events_option);

#ifdef CAML_INTERNALS

struct eventring_buffer_header {
  atomic_uint_fast64_t ring_head;
  atomic_uint_fast64_t ring_tail;
  uint64_t padding[8]; /* Padding so headers don't share cache lines */
};

struct eventring_metadata_header {
  uint64_t version;
  uint64_t max_domains;
  uint64_t ring_header_size_bytes; /* Ring buffer header size (bytes) */
  uint64_t ring_size_bytes; /* Ring data size (bytes) */
  uint64_t ring_size_elements; /* Ring size in 64-bit elements */
  uint64_t headers_offset; /* Offset from this struct to first header (bytes) */
  uint64_t data_offset; /* Offset from this struct to first data (byte) */
  uint64_t padding; /* Make the header a multiple of 64 bytes */
};

#define EVENTRING_NUM_ALLOC_BUCKETS 20
#define EVENTRING_MAX_MSG_LENGTH (1 << 10)

/* event header fields (for runtime events):
| -- length (10 bits) -- | runtime or user event (1 bit) | event type (4 bits) |
event id (13 bits)
*/

#define EVENTRING_ITEM_LENGTH(header) (((header) >> 54) & ((1UL << 10) - 1))
#define EVENTRING_ITEM_IS_RUNTIME(header) !((header) | (1UL << 53))
#define EVENTRING_ITEM_IS_USER(header) ((header) | (1UL << 53))
#define EVENTRING_ITEM_TYPE(header) (((header) >> 49) & ((1UL << 4) - 1))
#define EVENTRING_ITEM_ID(header) (((header) >> 36) & ((1UL << 13) - 1))

/* Set up eventring (and check if we need to start it immediately). Called
   from startup* */
void caml_eventring_init();

/* Destroy all allocated eventring structures and clear up the ring. Called
   from [caml_sys_exit] */
void caml_eventring_destroy();

/* Handle safely re-initialising the eventring structures in a forked child */
void caml_eventring_post_fork();

/* Returns the location of the eventring for the current process if started or
   NULL otherwise */
char_os* caml_eventring_current_location();

/* Functions for putting runtime data on to the eventring */
void caml_ev_begin(ev_runtime_phase phase);
void caml_ev_end(ev_runtime_phase phase);
void caml_ev_counter(ev_runtime_counter counter, uint64_t val);
void caml_ev_lifecycle(ev_lifecycle lifecycle, int64_t data);
void caml_ev_alloc(uint64_t sz);
void caml_ev_alloc_flush();
void caml_ev_flush();

#endif /* CAML_INTERNALS */

#endif /*CAML_EVENTRING_H*/
