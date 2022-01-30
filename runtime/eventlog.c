/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*                 Stephen Dolan, University of Cambridge                 */
/*                      Enguerrand Decorne, Tarides                       */
/*                                                                        */
/*   Copyright 2020 University of Cambridge                               */
/*   Copyright 2020 Tarides                                               */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#define CAML_INTERNALS
#include <stdio.h>
#include <string.h>
#include "caml/alloc.h"
#include "caml/eventlog.h"
#include "caml/misc.h"
#include "caml/memory.h"
#include "caml/osdeps.h"


#ifdef _WIN32
#include <wtypes.h>
#include <process.h>
#elif defined(HAS_UNISTD)
#include <unistd.h>
#endif

#ifdef HAS_MACH_ABSOLUTE_TIME
#include <mach/mach_time.h>
#elif HAS_POSIX_MONOTONIC_CLOCK
#include <time.h>
#endif

#ifdef CAML_INSTR

#define CTF_MAGIC 0xc1fc1fc1
#define CAML_TRACE_VERSION 0x100 /* Multicore OCaml WIP */

struct ctf_stream_header {
  uint32_t magic;
  uint16_t caml_trace_version;
  uint16_t stream_id;
};

static struct ctf_stream_header header = {
  CTF_MAGIC,
  CAML_TRACE_VERSION,
  0
};

#pragma pack(1)
struct ctf_event_header {
  uint64_t timestamp;
  uint32_t id;
};

struct event {
  struct ctf_event_header header;
  uint16_t  phase; /* for GC events */
  uint16_t  counter_kind; /* misc counter name */
  uint8_t  alloc_bucket; /* for alloc counters */
  uint64_t count; /* for misc counters */
  uint8_t is_backup_thread; /* is this event from a backup thread ? */
};

#define EVENT_BUF_SIZE 32768
struct event_buffer {
  uintnat ev_flushed;
  int domain_unique_id;
  uintnat ev_generated;
  uint64_t alloc_buckets [20]; /* allocation stats, by size */
  struct event events[EVENT_BUF_SIZE];
};

#define evbuf Caml_state->eventlog_buffer

static time_t startup_timestamp;
static atomic_uintnat eventlog_enabled = 0;
static atomic_uintnat eventlog_paused = 0;

static __thread uint8_t is_backup_thread = 0;

void caml_eventlog_is_backup_thread (void)
{
  is_backup_thread = 1;
}

static int64_t time_counter(void)
{
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
    if (mach_timebase_info (&time_base) != KERN_SUCCESS)
      return 0;

    if (time_base.denom == 0)
      return 0;
  }

  now = mach_absolute_time ();
  return (int64_t)((now * time_base.numer) / time_base.denom);

#elif defined(HAS_POSIX_MONOTONIC_CLOCK)
  struct timespec t;
  clock_gettime(CLOCK_MONOTONIC, &t);
  return
    (int64_t)t.tv_sec  * (int64_t)1000000000 +
    (int64_t)t.tv_nsec;


#endif
}

static void flush_events(FILE* out, struct event_buffer* eb);

void caml_eventlog_teardown(void)
{
  if(!evbuf) return;

  if (Caml_state->eventlog_out != NULL) {
    flush_events(Caml_state->eventlog_out, evbuf);
    fflush(Caml_state->eventlog_out);
    fclose(Caml_state->eventlog_out);
    Caml_state->eventlog_out = NULL;
  };

  free(evbuf);

  evbuf = NULL;
}

#define OUTPUT_FILE_LEN 4096
static void thread_setup_eventlog_file(int unique_id)
{
  char_os output_file[OUTPUT_FILE_LEN];
  char_os *eventlog_filename = NULL;
  long pid;

#ifdef _WIN32
  pid = _getpid();
#else
  pid = getpid();
#endif

  eventlog_filename = caml_secure_getenv(T("OCAML_EVENTLOG_PREFIX"));

  if (eventlog_filename) {
    int ret = snprintf_os(output_file, OUTPUT_FILE_LEN,
                          T("%s-caml-%ld-%d.eventlog"),
                          eventlog_filename, pid, unique_id);
    if (ret > OUTPUT_FILE_LEN)
      caml_fatal_error("eventlog: specified OCAML_EVENTLOG_PREFIX is too long");
  } else {
    snprintf_os(output_file, OUTPUT_FILE_LEN, T("caml-%ld-%d.eventlog"),
                pid, unique_id);
  }

  Caml_state->eventlog_out = fopen_os(output_file, T("wb"));

  if (Caml_state->eventlog_out) {
    int ret =  fwrite(&header, sizeof(struct ctf_stream_header),
                      1, Caml_state->eventlog_out);
    if (ret != 1)
      caml_eventlog_disable();
  } else {
    caml_fatal_error("eventlog: could not open trace for writing");
  }


}
#undef OUTPUT_FILE_LEN

static void thread_setup_evbuf()
{
  CAMLassert(!evbuf);

  evbuf = caml_stat_alloc_noexc(sizeof(*evbuf));

  if (evbuf == NULL)
    caml_fatal_error("eventlog: could not allocate event buffer");

  evbuf->ev_flushed = 0;
  evbuf->ev_generated = 0;
  evbuf->domain_unique_id = 2 * Caml_state->unique_id;

  Caml_state->eventlog_out = NULL;

  if (!Caml_state->eventlog_out)
    thread_setup_eventlog_file(evbuf->domain_unique_id);

  /* reset alloc_buckets */
  for (int i = 1; i < 20; i++) {
      evbuf->alloc_buckets[i] = 0;
  }

  return ;
}

#define FWRITE_EV(item, size) \
  if (fwrite(item, size, 1, out) != 1) \
    goto fwrite_failure;

#define FWRITE_HEADER(header, tid, is_bt) \
  FWRITE_EV(header, sizeof(struct ctf_event_header));\
  FWRITE_EV(tid, sizeof(uint32_t));\
  FWRITE_EV(is_bt, sizeof(uint8_t));

static void flush_events(FILE* out, struct event_buffer* eb)
{
  uintnat i;
  uint64_t flush_duration;
  uintnat n = evbuf->ev_generated;
  struct ctf_event_header header; // header for alloc event and flush events

  header.timestamp = time_counter() - startup_timestamp;

  for (i = 0; i < n; i++) {
    struct event ev = eb->events[i];

    FWRITE_HEADER(&ev.header, &eb->domain_unique_id, &ev.is_backup_thread);
    switch (ev.header.id) /* event payload */
    {
    case EV_ENTRY:
      FWRITE_EV(&ev.phase, sizeof(uint16_t));
      break;
    case EV_EXIT:
      FWRITE_EV(&ev.phase, sizeof(uint16_t));
      break;
    case EV_COUNTER:
      FWRITE_EV(&ev.count, sizeof(uint64_t));
      FWRITE_EV(&ev.counter_kind, sizeof(uint16_t));
      break;
    default:
      break;
    }
  }

  // flush alloc counters
  for (i = 1; i < 20; i++) {
    if (eb->alloc_buckets[i] != 0) {
      header.id = EV_ALLOC;
      FWRITE_HEADER(&header, &eb->domain_unique_id, &is_backup_thread);
      FWRITE_EV(&eb->alloc_buckets[i], sizeof(uint64_t));
      FWRITE_EV(&i, sizeof(uint8_t));
    };
    evbuf->alloc_buckets[i] = 0;
  };

  header.id = EV_FLUSH;
  flush_duration = (time_counter() - startup_timestamp) - header.timestamp;

  FWRITE_HEADER(&header, &eb->domain_unique_id, &is_backup_thread)
  FWRITE_EV(&flush_duration, sizeof(int64_t));

  return;

 fwrite_failure:
  /* on event flush failure, shut down eventlog. */
  if (caml_runtime_warnings_active())
    fprintf(stderr,
           "[ocaml] error while writing trace file, disabling eventlog\n");
  caml_eventlog_disable();
  return;

}

static void teardown_eventlog(void)
{
  if (evbuf) {
    if (Caml_state->eventlog_out)
      flush_events(Caml_state->eventlog_out, evbuf);
    caml_stat_free(evbuf);
    evbuf = NULL;
  }
  if (Caml_state->eventlog_out) {
    fclose(Caml_state->eventlog_out);
    Caml_state->eventlog_out = NULL;
  }
}

void caml_eventlog_init (void)
{
  char_os *toggle = caml_secure_getenv(T("OCAML_EVENTLOG_ENABLED"));

  if (toggle != NULL) {
    eventlog_enabled = 1;
    if (*toggle == 'p')
      atomic_store_rel(&eventlog_paused, 1);
  };

  if (!eventlog_enabled) return;

  startup_timestamp = time_counter();

  atexit(&teardown_eventlog);
}

static void post_event(ev_gc_phase phase, ev_gc_counter counter_kind,
                       uint8_t bucket, uint64_t count, ev_type ty)
{
  uintnat i;
  struct event* ev;

  if (!eventlog_enabled) return;
  if (eventlog_paused) return;
  if (!evbuf) thread_setup_evbuf();

  i = evbuf->ev_generated;
  CAMLassert(i <= EVENT_BUF_SIZE);
  if (i == EVENT_BUF_SIZE) {
    flush_events(Caml_state->eventlog_out, evbuf);
    evbuf->ev_generated = 0;
    i = 0;
  }
  ev = &evbuf->events[i];
  ev->header.id = ty;
  ev->count = count;
  ev->counter_kind = counter_kind;
  ev->alloc_bucket = bucket;
  ev->phase = phase;
  ev->is_backup_thread = is_backup_thread;
  ev->header.timestamp = time_counter() - startup_timestamp;
  evbuf->ev_generated = i + 1;
}

void caml_ev_begin(ev_gc_phase phase)
{
  post_event(phase, 0, 0, 0, EV_ENTRY);
}

void caml_ev_end(ev_gc_phase phase)
{
  post_event(phase, 0, 0, 0, EV_EXIT);
}

void caml_ev_counter(ev_gc_counter counter, uint64_t val)
{
  post_event(0, counter, 0, val, EV_COUNTER);
}

/* This function records allocations in caml_alloc_shr_aux in given bucket sizes
   These buckets are meant to be flushed explicitly by the caller through the
   caml_ev_alloc_flush function. Until then the buckets are just updated until
   flushed.
   TODO(engil): we actually need to record these in Multicore.
   Since the allocator is much different in Multicore it is unclear where and if
   we want these.
*/
void caml_ev_alloc(uint64_t sz)
{
  if (!eventlog_enabled) return;
  if (eventlog_paused) return;

  if (evbuf == NULL)
    thread_setup_evbuf();

  if (sz < 10) {
    ++evbuf->alloc_buckets[sz];
  } else if (sz < 100) {
    ++evbuf->alloc_buckets[sz/10 + 9];
  } else {
    ++evbuf->alloc_buckets[19];
  }
}

void caml_ev_flush (void)
{
  if (!eventlog_enabled) return;
  if (eventlog_paused) return;

  if (Caml_state->eventlog_out) {
    if (evbuf)
      flush_events(Caml_state->eventlog_out, evbuf);
    fflush(Caml_state->eventlog_out);
  };
}

void caml_eventlog_disable (void)
{
  atomic_store_rel(&eventlog_enabled, 0);
}

CAMLprim value caml_eventlog_resume(value v)
{
  CAMLassert(v == Val_unit);
  if (eventlog_enabled)
    atomic_store_rel(&eventlog_paused, 0);
  return Val_unit;
}

CAMLprim value caml_eventlog_pause(value v)
{
  CAMLassert(v == Val_unit);
  if (eventlog_enabled)
    atomic_store_rel(&eventlog_paused, 1);
  return Val_unit;
}

#else

CAMLprim value caml_eventlog_resume(value v)
{
  return Val_unit;
}

CAMLprim value caml_eventlog_pause(value v)
{
  return Val_unit;
}

#endif /*CAML_INSTR*/
