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

#include "caml/runtime_events.h"
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
#include <sys/stat.h>

#ifdef _WIN32
#include <process.h>
#include <processthreadsapi.h>
#include <wtypes.h>
#else
#include <sys/mman.h>
#endif


#if defined(HAS_UNISTD)
#include <unistd.h>
#endif

#define RUNTIME_EVENTS_VERSION 1

/*
This file contains the implementation for runtime_events's producer. The
consumer can be found in runtime_events_consumer.

Runtime_events is a transport for tracing and counter events from the OCaml
runtime. When enabled the caml_ev_* probes emit events that get written to
per-domain memory-mapped ring buffers. Consumers can be written that use the
OCaml or C apis to consume these events asynchronously. This can be done both
inside or outside the process.

The ring buffer is structured as a flight recorder, overwriting old data when
there is insufficient space to write new events. This enables users to
potentially only read the ring when some anomalous event occurs. No coordination
is needed with consumers who read the events - they detect races with the
producer and discard events when that happens.

The producer code is contained here . By default a <pid>.events file is
created in the current directory (overridable by setting
OCAML_RUNTIME_EVENTS_DIR). This file contains a ring buffer for each possible
domain (Max_domains). It is laid out in a structure that enables sparsity.
On-disk (or in-memory) footprint is proportional to the max number of concurrent
domains the process has ever run.

On disk structure:

----------------------------------------------------------------
| File header (version, offsets, etc..)                        |
----------------------------------------------------------------
| Ring 0..Max_domains metadata                                 |
| (head and tail indexes, one per cache line)                  |
----------------------------------------------------------------
| Ring 0..Max_domains data                                     |
| (actual ring data, default 2^16 words = 512k bytes)          |
----------------------------------------------------------------
| Custom event IDs                                             |
| 2^13 char[128] = 1M bytes                                    |
----------------------------------------------------------------
*/

typedef enum { EV_RUNTIME, EV_USER } ev_category;

/* These store state for the current ring buffers open for writing */
static struct runtime_events_metadata_header *current_metadata = NULL;
static int current_ring_total_size;
static char_os *runtime_events_path;
static char_os *current_ring_loc = NULL;

#ifdef _WIN32
static HANDLE ring_file_handle;
static HANDLE ring_handle;
#endif

/* This comes from OCAMLRUNPARAMs and is initialised in
   caml_runtime_events_init */
static int ring_size_words;

/* This is set if the OCAML_RUNTIME_EVENTS_PRESERVE environment
  variable is present and determines whether the ring buffer is
  cleaned up on program exit or not. It may be preserved to allow
  tooling to analyse very short running programs where there would
  be a race to read their ring buffers. */
static int preserve_ring = 0;

static atomic_uintnat runtime_events_enabled = 0;
static atomic_uintnat runtime_events_paused = 0;

static atomic_uintnat runtime_custom_event_index = 0;

/* List of globally known events. This is used to figure which event has a
   given string ID. */
static value user_events = Val_none;
static caml_plat_mutex user_events_lock;

/* Custom type write buffer */
static value write_buffer = Val_none;
static caml_plat_mutex write_buffer_lock;

static void write_to_ring(ev_category category, ev_message_type type,
                          int event_id, int event_length, uint64_t *content,
                          int word_offset);

static void events_register_write_buffer(int index, value event_name);
static void runtime_events_create_raw(void);

void caml_runtime_events_init(void) {

  caml_plat_mutex_init(&user_events_lock);
  caml_register_generational_global_root(&user_events);

  caml_plat_mutex_init(&write_buffer_lock);

  runtime_events_path = caml_secure_getenv(T("OCAML_RUNTIME_EVENTS_DIR"));

  if (runtime_events_path) {
    /* caml_secure_getenv's return shouldn't be cached */
    runtime_events_path = caml_stat_strdup_os(runtime_events_path);
  }

  ring_size_words = 1 << caml_params->runtime_events_log_wsize;

  preserve_ring =
            caml_secure_getenv(T("OCAML_RUNTIME_EVENTS_PRESERVE")) ? 1 : 0;

  if (caml_secure_getenv(T("OCAML_RUNTIME_EVENTS_START"))) {
    /* since [caml_runtime_events_init] can only be called from the startup code
    and we can be sure there is only a single domain running, it is safe to call
    [runtime_events_create_raw] outside of a stop-the-world section */
    runtime_events_create_raw();
  }
}

/* teardown the ring buffers. This must be called from a stop-the-world
   unless we are sure there is only a single domain running (e.g after a fork)
*/
static void runtime_events_teardown_raw(int remove_file) {
#ifdef _WIN32
    UnmapViewOfFile(current_metadata);
    CloseHandle(ring_file_handle);
    CloseHandle(ring_handle);

    if( remove_file ) {
      DeleteFile(current_ring_loc);
    }
#else
    /* This cast is necessary for compatibility with Illumos' non-POSIX
      mmap/munmap */
    munmap((void*)current_metadata, current_ring_total_size);

    if( remove_file ) {
      unlink(current_ring_loc);
    }
#endif

    caml_stat_free(current_ring_loc);
    current_metadata = NULL;

    atomic_store_release(&runtime_events_enabled, 0);
}

/* Stop-the-world which calls the teardown code */
static void stw_teardown_runtime_events(caml_domain_state *domain_state,
                               void *remove_file_data, int num_participating,
                               caml_domain_state **participating_domains) {
  caml_global_barrier();
  if (participating_domains[0] == domain_state) {
    int remove_file = *(int*)remove_file_data;
    runtime_events_teardown_raw(remove_file);
  }
  caml_global_barrier();
}


void caml_runtime_events_post_fork(void) {
  /* We are here in the child process after a call to fork (which can only
     happen when there is a single domain) and no mutator code that can spawn a
     new domain can have run yet. Let's be double sure. */
  CAMLassert(caml_domain_alone());

  if (atomic_load_acquire(&runtime_events_enabled)) {
    /* In the child we need to tear down the various structures used for the
    existing runtime_events from the parent. In doing so we need to make sure we
    don't remove the runtime_events file itself as that may still be used by
    the parent. There is no need for a stop-the-world in this case as we are
    certain there is only a single domain running. */
    runtime_events_teardown_raw(0 /* don't remove the file */);

    /* We still have the path and ring size from our parent */
    caml_runtime_events_start();
  }
}

/* Return the current location for the ring buffers of this process. This is
  used in the consumer to read the ring buffers of the current process */
char_os* caml_runtime_events_current_location(void) {
  if( atomic_load_acquire(&runtime_events_enabled) ) {
    return current_ring_loc;
  } else {
    return NULL;
  }
}

/* Write a lifecycle event and then trigger a stop the world to tear down the
  ring buffers */
void caml_runtime_events_destroy(void) {
  if (atomic_load_acquire(&runtime_events_enabled)) {
    write_to_ring(
      EV_RUNTIME, (ev_message_type){.runtime=EV_LIFECYCLE}, EV_RING_STOP, 0,
      NULL, 0);

    /* clean up runtime_events when we exit if we haven't been instructed to
      preserve the file. */
    int remove_file = preserve_ring ? 0 : 1;
    do {
      caml_try_run_on_all_domains(&stw_teardown_runtime_events,
                                  &remove_file, NULL);
    }
    while( atomic_load_acquire(&runtime_events_enabled) );
  }
}

/* Create the initial runtime_events ring buffers. This must be called from
  within a stop-the-world section if we cannot be sure there is only a single
  domain running. */
static void runtime_events_create_raw(void) {
  /* Don't initialise runtime_events twice */
  if (!atomic_load_acquire(&runtime_events_enabled)) {
    int ret, ring_headers_length, ring_data_length;
#ifdef _WIN32
    DWORD pid = GetCurrentProcessId();
#else
    int ring_fd;
    long int pid = getpid();
#endif

    current_ring_loc = caml_stat_alloc(RUNTIME_EVENTS_MAX_MSG_LENGTH);

    if (runtime_events_path) {
      snprintf_os(current_ring_loc, RUNTIME_EVENTS_MAX_MSG_LENGTH,
                  T("%s/%ld.events"), runtime_events_path, pid);
    } else {
      snprintf_os(current_ring_loc, RUNTIME_EVENTS_MAX_MSG_LENGTH,
                  T("%ld.events"), pid);
    }

    current_ring_total_size =
        RUNTIME_EVENTS_MAX_CUSTOM_EVENTS *
          sizeof(struct runtime_events_custom_event) +
        Max_domains * (ring_size_words * sizeof(uint64_t) +
                        sizeof(struct runtime_events_buffer_header)) +
        sizeof(struct runtime_events_metadata_header);

#ifdef _WIN32
    ring_file_handle = CreateFile(
      current_ring_loc,
      GENERIC_READ | GENERIC_WRITE,
      FILE_SHARE_READ | FILE_SHARE_WRITE,
      NULL,
      CREATE_ALWAYS,
      FILE_ATTRIBUTE_NORMAL,
      NULL
    );

    if (ring_file_handle == INVALID_HANDLE_VALUE) {
      char* ring_loc_u8 = caml_stat_strdup_of_os(current_ring_loc);
      caml_fatal_error("Couldn't open ring buffer loc: %s",
                        ring_loc_u8);
      caml_stat_free(ring_loc_u8);
    }

    ring_handle = CreateFileMapping(
      ring_file_handle,
      NULL,
      PAGE_READWRITE,
      0,
      current_ring_total_size,
      NULL
    );

    if (ring_handle == INVALID_HANDLE_VALUE) {
      caml_fatal_error("Could not create file mapping");
    }

    current_metadata = MapViewOfFile(
      ring_handle,
      FILE_MAP_ALL_ACCESS,
      0,
      0,
      0
    );

    if( current_metadata == NULL ) {
      caml_fatal_error("failed to map view of file");
    }
#else
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

    /* This cast is necessary for compatibility with Illumos' non-POSIX
      mmap/munmap */
    current_metadata = (struct runtime_events_metadata_header*)
                        mmap(NULL, current_ring_total_size,
                            PROT_READ | PROT_WRITE, MAP_SHARED, ring_fd, 0);

    if (current_metadata == NULL) {
      caml_fatal_error("Unable to mmap ring buffer");
    }

    close(ring_fd);
#endif
    ring_headers_length =
        Max_domains * sizeof(struct runtime_events_buffer_header);
    ring_data_length =
        Max_domains * ring_size_words * sizeof(uint64_t);

    current_metadata->version = RUNTIME_EVENTS_VERSION;
    current_metadata->max_domains = Max_domains;
    current_metadata->ring_header_size_bytes =
        sizeof(struct runtime_events_buffer_header);
    current_metadata->ring_size_bytes =
        ring_size_words * sizeof(uint64_t);
    current_metadata->ring_size_elements = ring_size_words;
    current_metadata->headers_offset =
        sizeof(struct runtime_events_metadata_header);

    /* strictly we can calculate this in a consumer but for simplicity we
        store it in the metadata header */
    current_metadata->data_offset =
      current_metadata->headers_offset + ring_headers_length;
    current_metadata->custom_events_offset =
      current_metadata->data_offset + ring_data_length;


    for (int domain_num = 0; domain_num < Max_domains; domain_num++) {
      /* we initialise each ring's metadata. We use the offset to the headers
        and then find the slot for each of domain in Max_domains */
      struct runtime_events_buffer_header *ring_buffer =
          (struct runtime_events_buffer_header
                *)((char *)current_metadata +
                  current_metadata->headers_offset +
                  domain_num * sizeof(struct runtime_events_buffer_header));

      ring_buffer->ring_head = 0;
      ring_buffer->ring_tail = 0;
    }

    // at the same instant: snapshot user_events list and set
    // runtime_events_enabled to 1
    caml_plat_lock(&user_events_lock);
    value current_user_event = user_events;
    atomic_store_release(&runtime_events_enabled, 1);
    caml_plat_unlock(&user_events_lock);

    atomic_store_release(&runtime_events_paused, 0);

    caml_ev_lifecycle(EV_RING_START, pid);


    while (Is_some (current_user_event)) {
      value event = Field(current_user_event, 0);
      events_register_write_buffer(Int_val(Field(event, 0)), Field(event, 1));
      current_user_event = Field(current_user_event, 1);
    }


  }
}

/* Stop the world section which calls [runtime_events_create_raw], used when we
   can't be sure there is only a single domain running. */
static void
stw_create_runtime_events(caml_domain_state *domain_state, void *data,
                              int num_participating,
                              caml_domain_state **participating_domains) {
  /* Everyone must be stopped for starting and stopping runtime_events */
  caml_global_barrier();

  /* Only do this on one domain */
  if (participating_domains[0] == domain_state) {
    runtime_events_create_raw();
  }
  caml_global_barrier();
}

CAMLprim value caml_runtime_events_start(void) {
  while (!atomic_load_acquire(&runtime_events_enabled)) {
    caml_try_run_on_all_domains(&stw_create_runtime_events, NULL, NULL);
  }

  return Val_unit;
}

CAMLprim value caml_runtime_events_pause(void) {
  if (!atomic_load_acquire(&runtime_events_enabled)) return Val_unit;

  uintnat not_paused = 0;

  if( atomic_compare_exchange_strong(&runtime_events_paused, &not_paused, 1) ) {
    caml_ev_lifecycle(EV_RING_PAUSE, 0);
  }

  return Val_unit;
}

CAMLprim value caml_runtime_events_resume(void) {
  if (!atomic_load_acquire(&runtime_events_enabled)) return Val_unit;

  uintnat paused = 1;

  if( atomic_compare_exchange_strong(&runtime_events_paused, &paused, 0) ) {
    caml_ev_lifecycle(EV_RING_RESUME, 0);
  }

  return Val_unit;
}
static struct runtime_events_buffer_header *get_ring_buffer_by_domain_id
                                                              (int domain_id) {
  return (
      struct runtime_events_buffer_header *)((char *)current_metadata +
                            current_metadata->headers_offset +
                            domain_id *
                                current_metadata->ring_header_size_bytes);
}

static void write_to_ring(ev_category category, ev_message_type type,
                          int event_id, int event_length, uint64_t *content,
                          int word_offset) {

  /* account for header and timestamp (which are both uint64) */
  uint64_t length_with_header_ts = event_length + 2;

  /* there is a ring buffer (made up of header and data) for each domain */
  struct runtime_events_buffer_header *domain_ring_header =
      get_ring_buffer_by_domain_id(Caml_state->id);

  /* get the pointer to the data for this domain's ring buffer */
  uint64_t *ring_ptr = (uint64_t *)((char*)current_metadata +
                                    current_metadata->data_offset
                        + Caml_state->id * current_metadata->ring_size_bytes);

  /* the head and tail indexes for the current domain's ring buffer (out of
      the header) */
  uint64_t ring_head = atomic_load_acquire(&domain_ring_header->ring_head);
  uint64_t ring_tail = atomic_load_acquire(&domain_ring_header->ring_tail);

  /* since rings can only be powers of two in size, we use this mask to cheaply
    convert the head and tail indexes in to the physical offset in the ring
    buffer's data. */
  uint64_t ring_mask = current_metadata->ring_size_elements - 1;
  uint64_t ring_tail_offset = ring_tail & ring_mask;

  /* we avoid writing events that straddle the end of the ring buffer */
  uint64_t ring_distance_to_end =
      current_metadata->ring_size_elements - ring_tail_offset;
  /* when we might write an event that is bigger than the physical size
    remaining we add a padding event instead and then write the actual
    event to the start of the ring buffer */
  uint64_t padding_required = 0;

  uint64_t timestamp = caml_time_counter();

  /* length must be less than 2^10 */
  CAMLassert(event_length < RUNTIME_EVENTS_MAX_MSG_LENGTH);
  /* Runtime event with type EV_INTERNAL and id 0 is reserved for padding */
  CAMLassert(
    !(category == EV_RUNTIME && type.runtime == EV_INTERNAL && event_id == 0));

  /* work out if padding is required */
  if (ring_distance_to_end < length_with_header_ts) {
    padding_required = ring_distance_to_end;
  }

  /* First we check if a write would take us over the head */
  while ((ring_tail + length_with_header_ts + padding_required) - ring_head >=
         ring_size_words) {
    /* The write would over-write some old bit of data. Need to advance the
      head. */
    uint64_t head_header = ring_ptr[ring_head & ring_mask];

    ring_head += RUNTIME_EVENTS_ITEM_LENGTH(head_header);

    // advance the ring head
    atomic_store_release(&domain_ring_header->ring_head, ring_head);
  }

  if (padding_required > 0) {
    ring_ptr[ring_tail_offset] =
        (ring_distance_to_end
         << 54); /* Padding header with size ring_distance_to_end
                    Readers will skip the message and go straight
                    to the beginning of the ring. */

    ring_tail += ring_distance_to_end;

    atomic_store_release(&domain_ring_header->ring_tail, ring_tail);

    ring_tail_offset = 0;
  }

  /* Below we write the header. See runtime_events.h for the layout structure
     of event headers.
  */

  ring_ptr[ring_tail_offset++] = RUNTIME_EVENTS_HEADER(
                                  length_with_header_ts,
                                  category == EV_RUNTIME,
                                  (type.runtime | type.user),
                                  event_id);

  ring_ptr[ring_tail_offset++] = timestamp;
  if (content != NULL) {
    memcpy(&ring_ptr[ring_tail_offset], content + word_offset,
           event_length * sizeof(uint64_t));
  }
  atomic_store_release(&domain_ring_header->ring_tail,
                       ring_tail + length_with_header_ts);
}

/* Functions for putting runtime data on to the runtime_events */

static inline int ring_is_active(void) {
    return
      atomic_load_relaxed(&runtime_events_enabled)
      && !atomic_load_relaxed(&runtime_events_paused);
}

void caml_ev_begin(ev_runtime_phase phase) {
  if ( ring_is_active() ) {
    write_to_ring(EV_RUNTIME, (ev_message_type){.runtime=EV_BEGIN}, phase, 0,
                  NULL, 0);
  }
}

void caml_ev_end(ev_runtime_phase phase) {
  if ( ring_is_active() ) {
    write_to_ring(EV_RUNTIME, (ev_message_type){.runtime=EV_EXIT}, phase, 0,
                  NULL, 0);
  }
}

void caml_ev_counter(ev_runtime_counter counter, uint64_t val) {
  if ( ring_is_active() ) {
    uint64_t buf[1];
    buf[0] = val;

    write_to_ring(
      EV_RUNTIME, (ev_message_type){.runtime=EV_COUNTER}, counter, 1, buf, 0);
  }
}

void caml_ev_lifecycle(ev_lifecycle lifecycle, int64_t data) {
  if ( ring_is_active() ) {
    write_to_ring(EV_RUNTIME, (ev_message_type){.runtime=EV_LIFECYCLE},
                  lifecycle, 1, (uint64_t *)&data, 0);
  }
}

static uint64_t alloc_buckets[RUNTIME_EVENTS_NUM_ALLOC_BUCKETS] = {
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};

void caml_ev_alloc(uint64_t sz) {
  if ( !ring_is_active() )
    return;

  if (sz < (RUNTIME_EVENTS_NUM_ALLOC_BUCKETS / 2)) {
    ++alloc_buckets[sz];
  } else if (sz < (RUNTIME_EVENTS_NUM_ALLOC_BUCKETS * 10 / 2)) {
    ++alloc_buckets[sz / (RUNTIME_EVENTS_NUM_ALLOC_BUCKETS / 2)
      + (RUNTIME_EVENTS_NUM_ALLOC_BUCKETS / 2 - 1)];
  } else {
    ++alloc_buckets[RUNTIME_EVENTS_NUM_ALLOC_BUCKETS - 1];
  }
}

void caml_ev_alloc_flush(void) {
  int i;

  if ( !ring_is_active() )
    return;

  write_to_ring(EV_RUNTIME, (ev_message_type){.runtime=EV_ALLOC}, 0,
                  RUNTIME_EVENTS_NUM_ALLOC_BUCKETS, alloc_buckets, 0);

  for (i = 1; i < RUNTIME_EVENTS_NUM_ALLOC_BUCKETS; i++) {
    alloc_buckets[i] = 0;
  }
}

/* Registers the [index] -> [event_name] mapping in the dedicated space in the
   ring buffer */
void events_register_write_buffer(int idx, value event_name) {
  struct runtime_events_custom_event *custom_event =
    &((struct runtime_events_custom_event *)
      ((char *)current_metadata + current_metadata->custom_events_offset))[idx];

  strncpy(custom_event->name, String_val(event_name),
          RUNTIME_EVENTS_CUSTOM_EVENT_ID_LENGTH - 1);
}

CAMLprim value caml_runtime_events_user_register(value event_name,
  value event_tag, value event_type)
{
  CAMLparam3(event_name, event_tag, event_type);
  CAMLlocal2(list_item, event);

  int index = atomic_fetch_add(&runtime_custom_event_index, 1);

  if (index > RUNTIME_EVENTS_MAX_CUSTOM_EVENTS) {
    caml_invalid_argument(
      "Runtime_events.User.register: maximum number of custom events exceeded");
  }

  int length = caml_string_length(event_name);
  if (length > RUNTIME_EVENTS_CUSTOM_EVENT_ID_LENGTH - 1) {
    caml_invalid_argument(
      "Runtime_events.User.register: maximum length for event name exceeded");
  }

  if (!caml_string_is_c_safe(event_name)) {
    caml_invalid_argument(
      "Runtime_events.User.register: event name has null characters");
  }

  // type 'a t = {
  //  id: int;
  //  name: string;
  //  typ: 'a Type.t;
  //  tag: tag;
  //}
  event = caml_alloc_small(4, 0);
  Field(event, 0) = Val_int(index);
  Field(event, 1) = event_name;
  Field(event, 2) = event_type;
  Field(event, 3) = event_tag;


  caml_plat_lock(&user_events_lock);
  // critical section: when we update the user_events list we need to make sure
  // it is not updated while we construct the pointer to the next element

  if (atomic_load_acquire(&runtime_events_enabled)) {
    // Ring buffer is already available, we register the name
    events_register_write_buffer(index, event_name);
  }

  // event is added to the list of known events
  list_item = caml_alloc_small(2, 0);
  Field(list_item, 0) = event;
  Field(list_item, 1) = user_events;
  caml_modify_generational_global_root(&user_events, list_item);
  // end critical section
  caml_plat_unlock(&user_events_lock);

  CAMLreturn(event);
}

CAMLprim value caml_runtime_events_user_write(value event, value event_content)
{
  CAMLparam2(event, event_content);
  CAMLlocal3(event_id, event_type, res);

  if ( !ring_is_active() )
    CAMLreturn(Val_unit);

  /* event type:
  type 'a t = {
    id: int;
    name: string;
    typ: 'a Type.t;
    tag: 'a tag option;
  }
  */
  event_id = Field(event, 0);

  event_type = Field(event, 2);
  /* event_type type:
  type 'a t =
  | Unit : unit t
  | Int : int t
  | Span : span t
  | Custom : 'a custom -> 'a t
  */

  // Check if event is custom or not.
  if (Is_block(event_type)) {
    // Custom { serialize; deserialize; id }
    value record = Field(event_type, 0);
    value serializer = Field(record, 0);

    caml_plat_lock(&write_buffer_lock);

    if (write_buffer == Val_none) {
      write_buffer = caml_alloc_string(RUNTIME_EVENTS_MAX_MSG_LENGTH);
      caml_register_generational_global_root(&write_buffer);
    }

    res = caml_callback2_exn(serializer, write_buffer, event_content);

    if (Is_exception_result(res)) {
      caml_plat_unlock(&write_buffer_lock);

      res = Extract_exception(res);
      caml_raise(res);
    }

    uintnat len_bytes = Int_val(res);
    uintnat len_64bit_word = (len_bytes + sizeof(uint64_t)) / sizeof(uint64_t);
    uintnat offset_index = len_64bit_word * sizeof(uint64_t) - 1;
    Bytes_val(write_buffer)[offset_index] = offset_index - len_bytes;
    write_to_ring(EV_USER, (ev_message_type){.user=EV_USER_MSG_TYPE_CUSTOM},
      Int_val(event_id), len_64bit_word, (uint64_t *) Bytes_val(write_buffer),
      0);

    caml_plat_unlock(&write_buffer_lock);

  } else {
    // Unit | Int | Span

    int event_type_id = Int_val(event_type);

    // Unit
    if (event_type_id == EV_USER_ML_TYPE_UNIT) {
      write_to_ring(EV_USER, (ev_message_type){.user=EV_USER_MSG_TYPE_UNIT},
        Int_val(event_id), 0, NULL, 0);
    }

    // Int
    if (event_type_id == EV_USER_ML_TYPE_INT) {
      uint64_t c_event_content = Int_val(event_content);
      write_to_ring(EV_USER, (ev_message_type){.user=EV_USER_MSG_TYPE_INT},
        Int_val(event_id), 1, &c_event_content, 0);
    }

    // Span
    if (event_type_id == EV_USER_ML_TYPE_SPAN) {
      // event_content type is Begin | End
      ev_user_message_type message_type;
      if (Int_val(event_content) == 0) {
        message_type = EV_USER_MSG_TYPE_SPAN_BEGIN;
      } else {
        message_type = EV_USER_MSG_TYPE_SPAN_END;
      }
      write_to_ring(EV_USER, (ev_message_type){.user=message_type},
        Int_val(event_id), 0, NULL, 0);
    }
  }

  CAMLreturn (Val_unit);
}

/* Find which event has the given name using the list of globally known events.
   If the event is not globally known but the type is one of the known types,
   then it can be partially reconstructed, the only missing information being
   the associated tag. This function returns an event structure, except when the
   event is unknown and the event type id is EV_USER_ML_TYPE_CUSTOM. */
CAMLprim value caml_runtime_events_user_resolve(
  char* event_name, ev_user_ml_type event_type_id)
{
  CAMLparam0();
  CAMLlocal3(event, cur_event_name, ml_event_name);

  // TODO: it might be possible to atomic load instead
  caml_plat_lock(&user_events_lock);
  value current_user_event = user_events;
  caml_plat_unlock(&user_events_lock);

  // which try to find an event with the matching name
  while (Is_some (current_user_event)) {
    event = Field(current_user_event, 0);
    cur_event_name = Field(event, 1);

    if (strncmp(String_val(cur_event_name), event_name,
                RUNTIME_EVENTS_CUSTOM_EVENT_ID_LENGTH) == 0) {
      CAMLreturn(event);
    }

    current_user_event = Field(current_user_event, 1);
  }

  if (event_type_id != EV_USER_ML_TYPE_CUSTOM) {
    // the event is not known, but its type is known
    // as we know the event type the event can be reconstructed
    value event_type = Val_int(event_type_id);
    uintnat event_name_len = strnlen(event_name,
                                      RUNTIME_EVENTS_CUSTOM_EVENT_ID_LENGTH);
    ml_event_name = caml_alloc_initialized_string(event_name_len, event_name);
    event = caml_runtime_events_user_register(ml_event_name, Val_none,
                                              event_type);

    CAMLreturn(event);
  }


  CAMLdrop;
  return (value) Val_none;
}
