/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*                          Sadiq Jaffer, Opsian                          */
/*                                                                        */
/*   Copyright 2021 Opsian Ltd                                            */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#define CAML_INTERNALS

#include "caml/eventring.h"
#include "caml/alloc.h"
#include "caml/callback.h"
#include "caml/custom.h"
#include "caml/fail.h"
#include "caml/memory.h"
#include "caml/mlvalues.h"
#include "caml/osdeps.h"
#include "caml/startup_aux.h"

#include <fcntl.h>
#include <stdatomic.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/stat.h>

#ifdef _WIN32
#include <process.h>
#include <wtypes.h>
#elif defined(HAS_UNISTD)
#include <unistd.h>
#endif

#ifdef HAS_MACH_ABSOLUTE_TIME
#include <mach/mach_time.h>
#elif HAS_POSIX_MONOTONIC_CLOCK
#include <time.h>
#endif

typedef enum { EV_RUNTIME, EV_USER } ev_category;

static char *eventring_path;
static struct eventring_metadata_header *current_metadata = NULL;
static char *current_ring_loc = NULL;
static int current_ring_total_size;

static int ring_size_words;

static atomic_uintnat eventring_enabled = 0;
static atomic_uintnat eventring_paused = 0;

static int64_t time_counter(void) {
#ifdef _WIN32
  static double clock_freq = 0;
  static LARGE_INTEGER now;

  if (clock_freq == 0) {
    LARGE_INTEGER f;
    if (!QueryPerformanceFrequency(&f))
      return 0;
    clock_freq = (1000000000.0 / f.QuadPart);
  };

  if (!QueryPerformanceCounter(&now))
    return 0;
  return (int64_t)(now.QuadPart * clock_freq);

#elif defined(HAS_MACH_ABSOLUTE_TIME)
  static mach_timebase_info_data_t time_base = {0};
  uint64_t now;

  if (time_base.denom == 0) {
    if (mach_timebase_info(&time_base) != KERN_SUCCESS)
      return 0;

    if (time_base.denom == 0)
      return 0;
  }

  now = mach_absolute_time();
  return (int64_t)((now * time_base.numer) / time_base.denom);

#elif defined(HAS_POSIX_MONOTONIC_CLOCK)
  struct timespec t;
  clock_gettime(CLOCK_MONOTONIC, &t);
  return (int64_t)t.tv_sec * (int64_t)1000000000 + (int64_t)t.tv_nsec;

#endif
}

static void write_to_ring(ev_category category, ev_message_type type,
                          int event_id, int event_length, uint64_t *content,
                          int word_offset);

void caml_eventring_init() {
  eventring_path = caml_secure_getenv(T("OCAML_EVENTRING_PREFIX"));

  ring_size_words = 1 << caml_params->eventring_size;

  if (caml_secure_getenv(T("OCAML_EVENTRING_START"))) {
    caml_eventring_start();
  }
}

static void teardown_eventring(caml_domain_state *domain_state, void *data,
                               int num_participating,
                               caml_domain_state **participanting_domains) {
  caml_global_barrier();
  if (participanting_domains[0] == domain_state) {
    munmap(current_metadata, current_ring_total_size);
    unlink(current_ring_loc);

    caml_stat_free(current_ring_loc);
    current_metadata = NULL;

    atomic_store_rel(&eventring_enabled, 0);
  }
  caml_global_barrier();
}

char* caml_eventring_current_location() {
  if( atomic_load_acq(&eventring_enabled) ) {
    return current_ring_loc;
  } else {
    return NULL;
  }
}

void caml_eventring_destroy() {
  if (atomic_load_acq(&eventring_enabled)) {
    write_to_ring(EV_RUNTIME, EV_LIFECYCLE, EV_RING_STOP, 0, NULL, 0);

    caml_try_run_on_all_domains(&teardown_eventring, NULL, NULL);
  }
}

static void
create_and_start_ring_buffers(caml_domain_state *domain_state, void *data,
                              int num_participating,
                              caml_domain_state **participanting_domains) {
  /* Everyone must be stopped for starting and stopping eventring */
  caml_global_barrier();

  /* Only do this on one domain */
  if (participanting_domains[0] == domain_state) {
    /* Don't initialise eventring twice */
    if (!atomic_load_acq(&eventring_enabled)) {
      int ring_fd, ret, ring_headers_length;
      long int pid;

      current_ring_loc = caml_stat_alloc(EVENTRING_MAX_MSG_LENGTH);

      pid = getpid();

      if (eventring_path) {
        snprintf_os(current_ring_loc, EVENTRING_MAX_MSG_LENGTH,
                    T("%s/%ld.eventring"), eventring_path, pid);
      } else {
        snprintf_os(current_ring_loc, EVENTRING_MAX_MSG_LENGTH,
                    T("%ld.eventring"), pid);
      }

      current_ring_total_size =
          Max_domains * (ring_size_words * sizeof(uint64_t) +
                         sizeof(struct eventring_buffer_header)) +
          sizeof(struct eventring_metadata_header);

      ring_fd =
          open(current_ring_loc, O_RDWR | O_CREAT, (S_IRUSR | S_IWUSR));

      if (ring_fd < 0) {
        caml_fatal_error("Couldn't open ring buffer loc: %s",
                         current_ring_loc);
      }

      ret = ftruncate(ring_fd, current_ring_total_size);

      if (ret < 0) {
        caml_fatal_error("Can't resize ring buffer");
      }

      current_metadata = mmap(NULL, current_ring_total_size,
                              PROT_READ | PROT_WRITE, MAP_SHARED, ring_fd, 0);

      ring_headers_length =
          Max_domains * sizeof(struct eventring_buffer_header);

      current_metadata->version = 1;
      current_metadata->max_domains = Max_domains;
      current_metadata->ring_header_size_bytes =
          sizeof(struct eventring_buffer_header);
      current_metadata->ring_size_bytes =
          ring_size_words * sizeof(uint64_t);
      current_metadata->ring_size_elements = ring_size_words;
      current_metadata->headers_offset =
          sizeof(struct eventring_metadata_header);
      /* strictly we can calculate this in a consumer but for simplicity we
         store it in the metadata header */
      current_metadata->data_offset =
        current_metadata->headers_offset + ring_headers_length;

      for (int domain_num = 0; domain_num < Max_domains; domain_num++) {
        struct eventring_buffer_header *ring_buffer =
            (struct eventring_buffer_header
                 *)((char *)current_metadata +
                    current_metadata->headers_offset +
                    domain_num * sizeof(struct eventring_buffer_header));

        ring_buffer->ring_head = 0;
        ring_buffer->ring_tail = 0;
      }

      close(ring_fd);

      atomic_store_rel(&eventring_enabled, 1);
      atomic_store_rel(&eventring_paused, 0);

      caml_ev_lifecycle(EV_RING_START, pid);
    }
  }
  caml_global_barrier();
}

CAMLprim value caml_eventring_start() {
  if (!atomic_load_acq(&eventring_enabled)) {
    caml_try_run_on_all_domains(&create_and_start_ring_buffers, NULL, NULL);
  }

  return Val_unit;
}

CAMLprim value caml_eventring_pause() {
  if (atomic_load_acq(&eventring_enabled) &&
      !atomic_load_acq(&eventring_paused)) {
    caml_ev_lifecycle(EV_RING_PAUSE, 0);
    atomic_store_rel(&eventring_paused, 1);
  }

  return Val_unit;
}

CAMLprim value caml_eventring_resume() {
  if (atomic_load_acq(&eventring_enabled) &&
      atomic_load_acq(&eventring_paused)) {
    caml_ev_lifecycle(EV_RING_RESUME, 0);
    atomic_store_rel(&eventring_paused, 0);
  }

  return Val_unit;
}
static struct eventring_buffer_header *get_ring_buffer_by_domain_id
                                                              (int domain_id) {
  return (
      struct eventring_buffer_header *)((char *)current_metadata +
                            current_metadata->headers_offset +
                            domain_id *
                                current_metadata->ring_header_size_bytes);
}

static void write_to_ring(ev_category category, ev_message_type type,
                          int event_id, int event_length, uint64_t *content,
                          int word_offset) {
  /* account for header and timestamp */
  uint64_t length_with_header_ts = event_length + 2;

  struct eventring_buffer_header *domain_ring_header =
      get_ring_buffer_by_domain_id(Caml_state->id);

  uint64_t *ring_ptr = (uint64_t *)((char*)current_metadata +
                                    current_metadata->data_offset
                        + Caml_state->id * current_metadata->ring_size_bytes);

  uint64_t ring_head = atomic_load_explicit(&domain_ring_header->ring_head,
                                            memory_order_acquire);
  uint64_t ring_tail = atomic_load_explicit(&domain_ring_header->ring_tail,
                                            memory_order_acquire);

  uint64_t ring_mask = current_metadata->ring_size_elements - 1;
  uint64_t ring_tail_offset = ring_tail & ring_mask;
  uint64_t ring_distance_to_end =
      current_metadata->ring_size_elements - ring_tail_offset;
  uint64_t padding_required = 0;

  uint64_t timestamp = time_counter();

  /* length must be less than 2^10 */
  CAMLassert(event_length < EVENTRING_MAX_MSG_LENGTH);
  /* Runtime event with type EV_INTERNAL and id 0 is reserved for padding */
  CAMLassert(!(category == EV_RUNTIME && type == EV_INTERNAL && event_id == 0));

  // Work out if padding is required
  if (ring_distance_to_end < length_with_header_ts) {
    padding_required = ring_distance_to_end;
  }

  // First we check if a write would take us over the head
  while ((ring_tail + length_with_header_ts + padding_required) - ring_head >=
         ring_size_words) {
    // The write would over-write some old bit of data. Need to advance the
    // head.
    uint64_t head_header = ring_ptr[ring_head & ring_mask];

    ring_head += EVENTRING_ITEM_LENGTH(head_header);

    atomic_store_explicit(&domain_ring_header->ring_head, ring_head,
                          memory_order_release); // advance the ring head
  }

  if (padding_required > 0) {
    ring_ptr[ring_tail_offset] =
        (ring_distance_to_end
         << 54); // Padding header with size ring_distance_to_end
                 // Readers will skip the message and go straight
                 // to the beginning of the ring.

    ring_tail += ring_distance_to_end;

    atomic_store_explicit(&domain_ring_header->ring_tail, ring_tail,
                          memory_order_release);

    ring_tail_offset = 0;
  }

  /* Below we write the header. To reiterate the event header structure:

    event header fields (for runtime events):

    length (10 bits)
    runtime or user event (1 bit)
    event type (4 bits)
    event id (13 bits)
  */

  ring_ptr[ring_tail_offset++] = (((uint64_t)length_with_header_ts) << 54) |
                                 ((category == EV_RUNTIME) ? 0 : (1ULL << 53)) |
                                 ((uint64_t)type) << 49 |
                                 ((uint64_t)event_id) << 36;

  ring_ptr[ring_tail_offset++] = timestamp;
  if (content != NULL) {
    memcpy(&ring_ptr[ring_tail_offset], content + word_offset,
           event_length * sizeof(uint64_t));
  }
  atomic_store_explicit(&domain_ring_header->ring_tail,
                        ring_tail + length_with_header_ts,
                        memory_order_release);
}

/* Functions for putting runtime data on to the eventring */

static inline int ring_is_active() {
    return
      atomic_load_explicit(&eventring_enabled, memory_order_relaxed)
      && !atomic_load_explicit(&eventring_paused, memory_order_relaxed);
}

void caml_ev_begin(ev_runtime_phase phase) {
  if ( ring_is_active() ) {
    write_to_ring(EV_RUNTIME, EV_BEGIN, phase, 0, NULL, 0);
  }
}

void caml_ev_end(ev_runtime_phase phase) {
  if ( ring_is_active() ) {
    write_to_ring(EV_RUNTIME, EV_EXIT, phase, 0, NULL, 0);
  }
}

void caml_ev_counter(ev_runtime_counter counter, uint64_t val) {
  if ( ring_is_active() ) {
    uint64_t buf[1];
    buf[0] = val;

    write_to_ring(EV_RUNTIME, EV_COUNTER, counter, 1, buf, 0);
  }
}

void caml_ev_lifecycle(ev_lifecycle lifecycle, int64_t data) {
  if ( ring_is_active() ) {
    write_to_ring(EV_RUNTIME, EV_LIFECYCLE, lifecycle, 1, (uint64_t *)&data, 0);
  }
}

static uint64_t alloc_buckets[EVENTRING_NUM_ALLOC_BUCKETS] = {
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};

/* This function records allocations in caml_alloc_shr_aux in given bucket sizes
   These buckets are meant to be flushed explicitly by the caller through the
   caml_ev_alloc_flush function. Until then the buckets are just updated until
   flushed.
*/
void caml_ev_alloc(uint64_t sz) {
  if ( !ring_is_active() )
    return;

  if (sz < (EVENTRING_NUM_ALLOC_BUCKETS / 2)) {
    ++alloc_buckets[sz];
  } else if (sz < (EVENTRING_NUM_ALLOC_BUCKETS * 10 / 2)) {
    ++alloc_buckets[sz / (EVENTRING_NUM_ALLOC_BUCKETS / 2)
      + (EVENTRING_NUM_ALLOC_BUCKETS / 2 - 1)];
  } else {
    ++alloc_buckets[EVENTRING_NUM_ALLOC_BUCKETS - 1];
  }
}

/*  Note that this function does not trigger an actual disk flush, it just
    pushes events in the event buffer.
*/
void caml_ev_alloc_flush() {
  int i;

  if ( !ring_is_active() )
    return;

  write_to_ring(EV_RUNTIME, EV_ALLOC, 0, EVENTRING_NUM_ALLOC_BUCKETS,
                                                              alloc_buckets, 0);

  for (i = 1; i < EVENTRING_NUM_ALLOC_BUCKETS; i++) {
    alloc_buckets[i] = 0;
  }
}

void caml_ev_flush() {
  // This is a no-op for eventring
}
