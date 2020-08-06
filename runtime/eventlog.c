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
#define CAML_TRACE_VERSION 0x1
#define CTF_METADATA_MAGIC 0x75D11D57
#define CTF_MAJOR_VERSION 1
#define CTF_MINOR_VERSION 8
#define MIN_AVAILABLE_EVENT_ID 5
#define CTF_TRACE_UUID {0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa}

/* The structs in this section are emitted structs, i.e. they
are emitted and writen to the files during the trace */

/* Header at the beginning of every packet in a stream */
struct ctf_stream_header {
  uint32_t magic;
  uint16_t caml_trace_version;
  uint8_t uuid[16];
  uint16_t stream_id;
};

/* Header for the eventlog 'data' stream */
static struct ctf_stream_header header = {
  CTF_MAGIC,
  CAML_TRACE_VERSION,
  CTF_TRACE_UUID,
  0
};

/* Special header for the 'metadata' stream */
#pragma pack(1)
struct ctf_metadata_header {
  uint32_t magic;
  uint8_t  uuid[16];
  uint32_t checksum;
  uint32_t content_size;
  uint32_t packet_size;
  uint8_t  compression_scheme;
  uint8_t  encryption_scheme;
  uint8_t  checksum_scheme;
  uint8_t  major;
  uint8_t  minor;
};

/* The fixed metadataa description for the GC events */
const char_os *fixed_metadata = "/* CTF 1.8 */\n\ntypealias integer {size = 8;}  := uint8_t;\ntypealias integer {size = 16;} := uint16_t;\ntypealias integer {size = 32;} := uint32_t;\ntypealias integer {size = 64;} := uint64_t;\n\nclock {\n    name = tracing_clock;\n    freq = 1000000000; /* tick = 1 ns */\n};\n\ntypealias integer {\n    size = 64;\n    map = clock.tracing_clock.value;\n} := tracing_clock_int_t;\n\n\n/*\n\nMain trace description,\nmajor and minor refers to the CTF version being used.\n\nThe packet header must contain at the very least\na stream id and the CTF magic number.\nWe only use one stream for now, and CTF magic number is 0xc1fc1fc1.\n\nWe add an extra field ocaml_trace_version to enable simpler transition if we add\nor remove metrics in the future.\n\n*/\ntrace {\n    major = 1;\n    minor = 8;\n    uuid = \"aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa\";\n    byte_order = le;\n    packet.header := struct {\n        uint32_t magic; /* required: must contain CTF magic number */\n        uint16_t ocaml_trace_version; /* our own trace format versioning */\n        uint8_t uuid[16];\n        uint16_t stream_id; /* required, although we have only one. */\n    };\n};\n\n/*\n\nWe use only one stream at the moment.\nEach event payload must contain a header with a timestamp and a pid.\nThe id field refers to the various event kinds defined further down this file.\n\n*/\nstream {\n    id = 0;\n    event.header := struct { /* for each event */\n        tracing_clock_int_t timestamp;\n        uint32_t id;\n    };\n    packet.context := struct {\n        uint32_t pid;\n    };\n};\n\n/*\n\nThese enumerations are mostly following the instrumented runtime datapoints.\ngc_phase aims to track the entry and exit time of each of the following events\nduring collection.\n\n*/\nenum gc_phase : uint16_t {\n    \"compact/main\" = 0,\n    \"compact/recompact\",\n    \"explicit/gc_set\",\n    \"explicit/gc_stat\",\n    \"explicit/gc_minor\",\n    \"explicit/gc_major\",\n    \"explicit/gc_full_major\",\n    \"explicit/gc_compact\",\n    \"major\",\n    \"major/roots\",\n    \"major/sweep\",\n    \"major/mark/roots\",\n    \"major/mark/main\",\n    \"major/mark/final\",\n    \"major/mark\",\n    \"major/mark/global_roots_slice\",\n    \"major_roots/global\",\n    \"major_roots/dynamic_global\",\n    \"major_roots/local\",\n    \"major_roots/C\",\n    \"major_roots/finalised\",\n    \"major_roots/memprof\",\n    \"major_roots/hook\",\n    \"major/check_and_compact\",\n    \"minor\",\n    \"minor/local_roots\",\n    \"minor/ref_tables\",\n    \"minor/copy\",\n    \"minor/update_weak\",\n    \"minor/finalized\",\n    \"explicit/gc_major_slice\"\n};\n\n/*\n\nMiscellaneous GC counters\n\n*/\nenum gc_counter : uint16_t {\n    \"alloc_jump\",\n    \"force_minor/alloc_small\",\n    \"force_minor/make_vect\",\n    \"force_minor/set_minor_heap_size\",\n    \"force_minor/weak\",\n    \"force_minor/memprof\",\n    \"major/mark/slice/remain\",\n    \"major/mark/slice/fields\",\n    \"major/mark/slice/pointers\",\n    \"major/work/extra\",\n    \"major/work/mark\",\n    \"major/work/sweep\",\n    \"minor/promoted\",\n    \"request_major/alloc_shr\",\n    \"request_major/adjust_gc_speed\",\n    \"request_minor/realloc_ref_table\",\n    \"request_minor/realloc_ephe_ref_table\",\n    \"request_minor/realloc_custom_table\"\n};\n\n/*\n\nBlock allocation counters, per size buckets.\n\n*/\nenum alloc_bucket : uint8_t {\n  \"alloc 01\" = 1,\n  \"alloc 02\",\n  \"alloc 03\",\n  \"alloc 04\",\n  \"alloc 05\",\n  \"alloc 06\",\n  \"alloc 07\",\n  \"alloc 08\",\n  \"alloc 09\",\n  \"alloc 10-19\",\n  \"alloc 20-29\",\n  \"alloc 30-39\",\n  \"alloc 40-49\",\n  \"alloc 50-59\",\n  \"alloc 60-69\",\n  \"alloc 70-79\",\n  \"alloc 80-89\",\n  \"alloc 90-99\",\n  \"alloc large\"\n};\n\n/*\n\nEach event is comprised of the previously defined event.header\nand the fields defined here.\n\nAn entry event marks the start of a gc phase.\n\n*/\nevent {\n    id = 0;\n    name = \"entry\";\n    stream_id = 0;\n    fields := struct {\n        enum gc_phase phase;\n    };\n};\n\n/*\n\nexit counterparts to entry events\n\n*/\nevent {\n    id = 1;\n    name = \"exit\";\n    stream_id = 0;\n    fields := struct {\n        enum gc_phase phase;\n    };\n};\n\nevent {\n    id = 2;\n    name = \"counter\";\n    stream_id = 0;\n    fields := struct {\n        uint64_t count;\n        enum gc_counter kind;\n    };\n};\n\nevent {\n    id = 3;\n    name = \"alloc\";\n    stream_id = 0;\n    fields := struct {\n        uint64_t count;\n        enum alloc_bucket bucket;\n    };\n};\n\n/*\n Flush events are used to track the time spent by the tracing runtime flushing\n data to disk, useful to remove flushing overhead for other runtime mesurements\n in the trace.\n*/\nevent {\n     id = 4;\n     name = \"flush\";\n     stream_id = 0;\n     fields := struct {\n        uint64_t duration;\n     };\n};\n";
/* Description of a user event for the 'metadata' stream */
const char_os *user_event_format = "\nevent {\n     id = %d;\n     name = \"%s\";\n     stream_id = 0;\n     fields := struct {\n        uint8_t span_type;\n     };\n};\n";
static struct ctf_metadata_header metadata_header = {
  CTF_METADATA_MAGIC,
  CTF_TRACE_UUID,
  0,
  0,
  0,
  0,
  0,
  0,
  CTF_MAJOR_VERSION,
  CTF_MINOR_VERSION
};

/* Header at the beginning of every event in the 'data' stream */
#pragma pack(1)
struct ctf_event_header {
  uint64_t timestamp;
  uint32_t id;
};

/* The structs in this section are programmatic structs, i.e. they
are used in the code but not directly emitted as is to the trace files. */

struct event {
  struct ctf_event_header header;
  uint16_t  phase; /* for GC events */
  uint16_t  counter_kind; /* misc counter name */
  uint8_t  alloc_bucket; /* for alloc counters */
  uint64_t count; /* for misc counters */
  uint8_t span_type; /* for user events */
};

#define EVENT_BUF_SIZE 4096
struct event_buffer {
  uintnat ev_generated;
  struct event events[EVENT_BUF_SIZE];
};

static struct event_buffer* evbuf;

/* Struct definitions over */

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

  if (time_base.denom == 0) {
    if (mach_timebase_info (&time_base) != KERN_SUCCESS)
      return 0;

    if (time_base.denom == 0)
      return 0;
  }

  uint64_t now = mach_absolute_time ();
  return (int64_t)((now * time_base.numer) / time_base.denom);

#elif defined(HAS_POSIX_MONOTONIC_CLOCK)
  struct timespec t;
  clock_gettime(CLOCK_MONOTONIC, &t);
  return
    (int64_t)t.tv_sec  * (int64_t)1000000000 +
    (int64_t)t.tv_nsec;


#endif
}

static void setup_evbuf()
{
  CAMLassert(!evbuf);
  evbuf = caml_stat_alloc_noexc(sizeof(*evbuf));

  if (evbuf == NULL)
    caml_fatal_error("eventlog: could not allocate event buffer");

  evbuf->ev_generated = 0;
}

#define OUTPUT_FILE_LEN 4096
static void get_trace_filename(char_os *output_file, const char_os *file_type) {
  char_os *eventlog_filename = NULL;
  eventlog_filename = caml_secure_getenv(T("OCAML_EVENTLOG_FILE"));

  if (eventlog_filename) {
    int ret = snprintf_os(output_file, OUTPUT_FILE_LEN, T("%s.%d.%s"),
                         eventlog_filename, Caml_state->eventlog_startup_pid, file_type);
    if (ret > OUTPUT_FILE_LEN)
      caml_fatal_error("eventlog: specified OCAML_EVENTLOG_FILE is too long");
  } else {
    snprintf_os(output_file, OUTPUT_FILE_LEN, T("caml-%s-%d"),
               file_type, Caml_state->eventlog_startup_pid);
  }
}

static void setup_eventlog_file()
{
  char_os output_file[OUTPUT_FILE_LEN];
  get_trace_filename(output_file, "eventlog");

  Caml_state->eventlog_out = fopen_os(output_file, T("wb"));

  if (Caml_state->eventlog_out) {
    /* Write the packet header */
    int ret =  fwrite(&header, sizeof(struct ctf_stream_header),
                      1, Caml_state->eventlog_out);
    if (ret != 1)
      caml_eventlog_disable();
    /* Write the packet context, which is just the PID */
    else if (fwrite(&Caml_state->eventlog_startup_pid, sizeof(uint32_t), 1, Caml_state->eventlog_out) != 1)
      caml_eventlog_disable();
    fflush(Caml_state->eventlog_out);
  } else {
    caml_fatal_error("eventlog: could not open trace for writing");
  }
}

static void setup_metadata_file(void)
{
  char_os output_file[OUTPUT_FILE_LEN];
  get_trace_filename(output_file, "metadata");

  Caml_state->ctf_metadata_file = fopen_os(output_file, T("wb"));

  if (Caml_state->ctf_metadata_file) {
    /* 'content size' is the size of the metadata header and the payload in bits */
    metadata_header.content_size = (strlen(fixed_metadata) + sizeof(struct ctf_metadata_header)) * __CHAR_BIT__;
    /* 'packet size' is the same as 'content size', just including any padding bits, which we don't use */
    metadata_header.packet_size = metadata_header.content_size;

    int ret = fwrite(&metadata_header, sizeof(struct ctf_metadata_header),
                      1, Caml_state->ctf_metadata_file);
    if (ret != 1) {
      fprintf(stderr,
           "[ocaml] error while writing metadata header to trace file, disabling eventlog\n");
      caml_eventlog_disable();
    }

    char_os *remaining_metadata = fixed_metadata;
    int remaining_bytes = strlen(fixed_metadata);
    while (remaining_bytes > 0) {
      int bytes_written = fprintf(Caml_state->ctf_metadata_file, "%s", remaining_metadata);
      remaining_metadata += bytes_written;
      remaining_bytes -= bytes_written;
    }
    
    fflush(Caml_state->ctf_metadata_file);
  } else {
    caml_fatal_error("eventlog: could not open metadata file for writing");
  }

  Caml_state->ctf_user_event_id = MIN_AVAILABLE_EVENT_ID;
}
#undef OUTPUT_FILE_LEN

#define FWRITE_EV(item, size) \
  if (fwrite(item, size, 1, out) != 1) \
    goto fwrite_failure;

static void flush_events(FILE* out, struct event_buffer* eb)
{
  uintnat i;
  uint64_t flush_duration;
  uintnat n = eb->ev_generated;

  struct ctf_event_header ev_flush;
  ev_flush.id = EV_FLUSH;
  ev_flush.timestamp = time_counter() -
                        Caml_state->eventlog_startup_timestamp;

  for (i = 0; i < n; i++) {
    struct event ev = eb->events[i];

    FWRITE_EV(&ev.header, sizeof(struct ctf_event_header));

    switch (ev.header.id)
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
    case EV_ALLOC:
      FWRITE_EV(&ev.count, sizeof(uint64_t));
      FWRITE_EV(&ev.alloc_bucket, sizeof(uint8_t));
      break;
    default:
      FWRITE_EV(&ev.span_type, sizeof(uint8_t));
      break;
    }
  }

  flush_duration =
    (time_counter() - Caml_state->eventlog_startup_timestamp) -
    ev_flush.timestamp;

  FWRITE_EV(&ev_flush, sizeof(struct ctf_event_header));
  FWRITE_EV(&flush_duration, sizeof(uint64_t));

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
  if (Caml_state->ctf_metadata_file) {
    fclose(Caml_state->ctf_metadata_file);
    Caml_state->ctf_metadata_file = NULL;
  }
}

void caml_eventlog_init()
{
  char_os *toggle = caml_secure_getenv(T("OCAML_EVENTLOG_ENABLED"));

  if (toggle != NULL) {
    Caml_state->eventlog_enabled = 1;
    if (*toggle == 'p')
      Caml_state->eventlog_paused = 1;
  };

  if (!Caml_state->eventlog_enabled) return;

  Caml_state->eventlog_startup_timestamp = time_counter();
#ifdef _WIN32
  Caml_state->eventlog_startup_pid = _getpid();
#else
  Caml_state->eventlog_startup_pid = getpid();
#endif

  setup_eventlog_file();
  setup_metadata_file();
  setup_evbuf();

  atexit(&teardown_eventlog);
}

static void post_event(ev_gc_phase phase, ev_gc_counter counter_kind,
                       uint8_t bucket, uint64_t count, ev_user_type span_type, ev_type ty)
{
  uintnat i;
  struct event* ev;

  if (!Caml_state->eventlog_enabled) return;
  if (Caml_state->eventlog_paused) return;

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
  ev->span_type = span_type;
  ev->header.timestamp = time_counter() -
                           Caml_state->eventlog_startup_timestamp;
  evbuf->ev_generated = i + 1;
}

void caml_ev_begin(ev_gc_phase phase)
{
  post_event(phase, 0, 0, 0, 0, EV_ENTRY);
}

void caml_ev_end(ev_gc_phase phase)
{
  post_event(phase, 0, 0, 0, 0, EV_EXIT);
}

void caml_ev_counter(ev_gc_counter counter, uint64_t val)
{
  post_event(0, counter, 0, val, 0, EV_COUNTER);
}

static uint64_t alloc_buckets [20] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};

/* This function records allocations in caml_alloc_shr_aux in given bucket sizes
   These buckets are meant to be flushed explicitly by the caller through the
   caml_ev_alloc_flush function. Until then the buckets are just updated until
   flushed.
*/
void caml_ev_alloc(uint64_t sz)
{
  if (!Caml_state->eventlog_enabled) return;
  if (Caml_state->eventlog_paused) return;

  if (sz < 10) {
    ++alloc_buckets[sz];
  } else if (sz < 100) {
    ++alloc_buckets[sz/10 + 9];
  } else {
    ++alloc_buckets[19];
  }
}

/*  Note that this function does not trigger an actual disk flush, it just
    pushes events in the event buffer.
*/
void caml_ev_alloc_flush()
{
  int i;

  if (!Caml_state->eventlog_enabled) return;
  if (Caml_state->eventlog_paused) return;

  for (i = 1; i < 20; i++) {
    if (alloc_buckets[i] != 0) {
      post_event(0, 0, i, alloc_buckets[i], 0, EV_ALLOC);
    };
    alloc_buckets[i] = 0;
  }
}

void caml_ev_flush()
{
  if (!Caml_state->eventlog_enabled) return;
  if (Caml_state->eventlog_paused) return;

  if (Caml_state->eventlog_out) {
    if (evbuf)
      flush_events(Caml_state->eventlog_out, evbuf);
    fflush(Caml_state->eventlog_out);
  };
}

void caml_eventlog_disable()
{
  Caml_state->eventlog_enabled = 0;
  teardown_eventlog();
}

CAMLprim value caml_eventlog_resume(value v)
{
  CAMLassert(v == Val_unit);
  if (Caml_state->eventlog_enabled)
    Caml_state->eventlog_paused = 0;
  return Val_unit;
}

CAMLprim value caml_eventlog_pause(value v)
{
  CAMLassert(v == Val_unit);
  if (Caml_state->eventlog_enabled) {
    Caml_state->eventlog_paused = 1;
    if (evbuf && Caml_state->eventlog_out)
      flush_events(Caml_state->eventlog_out, evbuf);
  };
  return Val_unit;
}

#define MAX_EVENT_SIZE 4096
CAMLprim value caml_eventlog_new_event(value v)
{ 
  const char *new_name = String_val(v);
  char_os new_event[MAX_EVENT_SIZE];
  if (snprintf_os(new_event, MAX_EVENT_SIZE, user_event_format,
      Caml_state->ctf_user_event_id, new_name) > MAX_EVENT_SIZE) {
    fprintf(stderr,
      "[ocaml] specified user event name in eventlog tracing is too long");
    caml_eventlog_disable();
  }
  
  metadata_header.content_size = (strlen(new_event) + sizeof(struct ctf_metadata_header)) * __CHAR_BIT__;
  metadata_header.packet_size = metadata_header.content_size;
  /* Write the metadata header */
  int ret = fwrite(&metadata_header, sizeof(struct ctf_metadata_header),
              1, Caml_state->ctf_metadata_file);
  if (ret != 1) {
    fprintf(stderr,
      "[ocaml] error while writing metadata header to trace file, disabling eventlog\n");
    caml_eventlog_disable();
  }

  char_os *remaining_metadata = new_event;
  int remaining_bytes = strlen(new_event);
  /* Write the metadata payload, which is the description of the new user event */
  while (remaining_bytes > 0) {
    int bytes_written = fprintf(Caml_state->ctf_metadata_file, "%s", remaining_metadata);
    remaining_metadata += bytes_written;
    remaining_bytes -= bytes_written;
  } 
  fflush(Caml_state->ctf_metadata_file);

  return Val_int(Caml_state->ctf_user_event_id++);
}
#undef MAX_EVENT_SIZE

CAMLprim value caml_eventlog_emit_begin_event(value v)
{
  int cur_event_id = Int_val(v);
  post_event(0, 0, 0, 0, EV_USER_BEGIN, cur_event_id);
  return Val_unit;
}

CAMLprim value caml_eventlog_emit_end_event(value v)
{
  int cur_event_id = Int_val(v);
  post_event(0, 0, 0, 0, EV_USER_END, cur_event_id);
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

CAMLprim value caml_eventlog_new_event(value v)
{
  return Val_unit;
}

CAMLprim value caml_eventlog_emit_begin_event(value v)
{
  return Val_unit;
}

CAMLprim value caml_eventlog_emit_end_event(value v)
{
  return Val_unit;
}

#endif /*CAML_INSTR*/
