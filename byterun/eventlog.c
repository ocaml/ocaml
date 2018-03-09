#define CAML_INTERNALS

#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <errno.h>
#include <stdlib.h>

#include "caml/config.h"
#ifdef HAS_GETTIMEOFDAY
#include <sys/time.h>
#endif

#include "caml/mlvalues.h"
#include "caml/eventlog.h"
#include "caml/startup.h"
#include "caml/domain.h"
#include "caml/platform.h"

struct event_details {
  int type;
  int payload_size;
  const char* desc;
};
enum {
  sz_capset = 4,
  sz_capset_type = 2,
  sz_block_length = 4,
  sz_timestamp = 8,
  sz_cap = 2,
  sz_tid = 4,
  sz_stop_status = 2
};

static const struct event_details
  ev_capset_create = { 25, (sz_capset + sz_capset_type), "Create capability set" },
  ev_block_marker = { 18, (sz_block_length + sz_timestamp + sz_cap), "Block marker" },
  ev_request_par_gc = { 12, 0, "Request global GC" },
  ev_gc_start = { 9, 0, "Starting GC" },
  ev_gc_end = { 10, 0, "Finished GC" },
  ev_run_thread = { 1, sz_tid, "Run thread" },
  ev_stop_thread = { 2, (sz_tid + sz_stop_status + sz_tid), "Stop thread" },
  ev_wakeup_thread = { 8, (sz_tid + sz_cap), "Wakeup thread" },
  ev_user_msg = { 19, 0xffff, "User message" };

static const struct event_details* all_events[] = {
  &ev_capset_create,
  &ev_block_marker,
  &ev_request_par_gc,
  &ev_gc_start,
  &ev_gc_end,
  &ev_run_thread,
  &ev_stop_thread,
  &ev_wakeup_thread,
  &ev_user_msg
};

static caml_plat_mutex lock = CAML_PLAT_MUTEX_INITIALIZER;
static FILE* output;
static int num_users;
static uint64_t initial_timestamp;

#define EVENT_BUFFER_SIZE 8192
struct event_buffer {
  char* buffer;
  char* pos;
  mlsize_t remaining;
};

static __thread struct event_buffer evbuf;


static uint64_t timestamp()
{
  uint64_t now = 0;
#ifdef HAS_GETTIMEOFDAY
  struct timeval tv;
  gettimeofday(&tv, NULL);
  now = tv.tv_sec;
  now *= 1000000;
  now += tv.tv_usec;
  now *= 1000;
#endif
  return now;
}

static void output_event_descriptions();
static void output_initial_events();
void caml_setup_eventlog() {
  char filename[200 + sizeof(".eventlog")];
#if !defined(HAS_GETTIMEOFDAY)
  caml_fatal_error("No gettimeofday() on this system, event logging doesn't work");
#endif

  if (!caml_params->eventlog_enabled) return;

  caml_plat_lock(&lock);
  num_users++;
  if (!output) {
    sprintf(filename, "%.200s.eventlog",
            caml_params->exe_name ? caml_params->exe_name : "program");
    output = fopen(filename, "w");
    if (!output) {
      caml_fatal_error_arg2("Failed to open event log '%s'", filename,
                            ": %s", strerror(errno));
    }
    caml_gc_log("Logging events to '%s'", filename);
    initial_timestamp = timestamp();
    output_event_descriptions();
    output_initial_events();
  }
  caml_plat_unlock(&lock);
  atexit(&caml_teardown_eventlog);
}

static char* write_8(char* p, char x)
{
  p[0] = x;
  return p + 1;
}

static char* write_16(char* p, int x)
{
  p[0] = (unsigned char)(x >> 8);
  p[1] = (unsigned char)x;
  return p + 2;
}

static char* write_32(char* p, uint32_t x)
{
  p[0] = (unsigned char)(x >> 24);
  p[1] = (unsigned char)(x >> 16);
  p[2] = (unsigned char)(x >> 8);
  p[3] = (unsigned char)x;
  return p + 4;
}

static char* write_timestamp(char* p)
{
  int i;
  uint64_t now_ns = timestamp() - initial_timestamp;
  for (i = 64 - 8; i >= 0; i -= 8) {
    *p++ = (unsigned char)(now_ns >> i);
  }
  return p;
}

static char* write_string(char* p, const char* s)
{
  size_t n = strlen(s);
  memcpy(p, s, n);
  return p + n;
}

static void output_buffer(char* buf, mlsize_t len)
{
  mlsize_t written = fwrite(buf, 1, len, output);
  if (written != len) {
    caml_gc_log("fwrite() failed, wrote %lu instead of %lu bytes",
                (unsigned long)written, (unsigned long)len);
  }
  if (fflush(output)) {
    caml_gc_log("fflush() failed: %s", strerror(errno));
  }
}

static void output_event_descriptions()
{
  int i;
  char* buf = malloc(EVENT_BUFFER_SIZE);
  char* p = buf;

  p = write_string(p, "hdrb"); /* HEADER_BEGIN */
  p = write_string(p, "hetb"); /* HET_BEGIN */
  for (i = 0; i < sizeof(all_events) / sizeof(all_events[0]); i++) {
    struct event_details ev = *all_events[i];
    p = write_string(p, "etb"); p = write_8(p, 0); /* ET_BEGIN */
    p = write_16(p, ev.type);
    p = write_16(p, ev.payload_size);
    p = write_32(p, strlen(ev.desc));
    p = write_string(p, ev.desc);
    p = write_32(p, 0);
    p = write_string(p, "ete"); p = write_8(p, 0); /* ET_END */
  }
  p = write_string(p, "hete"); /* HET_END */
  p = write_string(p, "hdre"); /* HEADER_END */

  p = write_string(p, "datb");
  output_buffer(buf, p - buf);
  free(buf);
}

void flush_eventlog()
{
  if (evbuf.buffer && evbuf.remaining < EVENT_BUFFER_SIZE) {
    caml_gc_log("Flushing event log");
    uint32_t len = (uint32_t)(evbuf.pos - evbuf.buffer);
    char* p = evbuf.buffer;
    p += 2;  /* skip EVENT_BLOCK_MARKER */
    p += 8;  /* skip timestamp */
    p = write_32(p, len);
    p = write_timestamp(p);
    output_buffer(evbuf.buffer, len);
  }
  if (output) {
    fflush(output);
  }
}

static char* append_data(int size) {
  char* p;
  if (size > evbuf.remaining) {
    flush_eventlog();
    if (!evbuf.buffer)
      evbuf.buffer = malloc(EVENT_BUFFER_SIZE);
    /* add block header */
    p = evbuf.buffer;
    p = write_16(p, ev_block_marker.type);
    p = write_timestamp(p);
    p = write_32(p, 0);     /* overwritten later */
    p = write_timestamp(p); /* overwritten later */
    p = write_16(p, Caml_state->id);
    evbuf.pos = p;
    evbuf.remaining = EVENT_BUFFER_SIZE - (p - evbuf.buffer);
  }
  Assert(evbuf.remaining + evbuf.pos == evbuf.buffer + EVENT_BUFFER_SIZE);
  p = evbuf.pos;
  evbuf.pos += size;
  evbuf.remaining -= size;
  return p;
}

static char* append_event(const struct event_details* ev)
{
  int event_size = 2 + 8 + ev->payload_size;
  char* p = append_data(event_size);
  p = write_16(p, ev->type);
  p = write_timestamp(p);
  return p;
}

static void output_initial_events()
{
  /* Write EVENT_CAPSET_CREATE */
  char* p;
  p = append_event(&ev_capset_create);
  p = write_32(p, 0);
  p = write_16(p, 2 /* CAPSET_TYPE_OSPROCESS */);

  p = append_event(&ev_capset_create);
  p = write_32(p, 0);
  p = write_16(p, 3 /* CAPSET_TYPE_CLOCKDOMAIN */);
}

void caml_ev_start_gc()
{
  if (!output) return;
  /* Calls to `caml_handle_incoming_interrupts` from
   * `caml_major_collection_slice` may invoke a GC. To avoid producing nested
   * GC events, we keep track of the nesting depth. */
  if (Caml_state->gc_event_nesting_depth == 0)
    append_event(&ev_gc_start);
  Caml_state->gc_event_nesting_depth++;
}

void caml_ev_end_gc()
{
  if (!output) return;
  Caml_state->gc_event_nesting_depth--;
  if (Caml_state->gc_event_nesting_depth == 0)
    append_event(&ev_gc_end);
}

void caml_ev_request_stw()
{
  if (!output) return;
  append_event(&ev_request_par_gc);
}

void caml_ev_pause(long reason)
{
  char* p;
  if (!output) return;
  p = append_event(&ev_stop_thread);
  p = write_32(p, Caml_state->id);
  if (reason == EV_PAUSE_GC) {
    p = write_16(p, 1 /* HeapOverflow */);
    p = write_32(p, 0 /* unused */);
  } else if (reason == EV_PAUSE_BLOCK) {
    p = write_16(p, 14 /* BlockedOnCCall */);
    p = write_32(p, 0 /* Unused */);
  } else if (reason == EV_PAUSE_TERMINATE) {
    p = write_16(p, 5 /* ThreadFinished */);
    p = write_32(p, 0 /* Unused */);
  } else if (reason == EV_PAUSE_YIELD) {
    p = write_16(p, 3 /* ThreadYielding */);
    p = write_32(p, 0 /* Unused */);
  } else {
    /* reason == EV_PAUSE_RPC(dom) */
    p = write_16(p, 8 /* BlockedOnBlackHole */);
    p = write_32(p, reason);
  }
}

void caml_ev_resume()
{
  char* p;
  if (!output) return;
  p = append_event(&ev_run_thread);
  p = write_32(p, Caml_state->id);
}

void caml_ev_wakeup(struct domain* dom)
{
  char* p;
  if (!output) return;
  p = append_event(&ev_wakeup_thread);
  p = write_32(p, dom->state->id);
  p = write_32(p, dom->state->id);
}

void caml_ev_msg(char* format, ...)
{
  char buffer[512];
  va_list args;
  va_start (args, format);
  char* p;
  int len;
  if (!output) return;
  vsprintf(buffer, format, args);
  len = strlen(buffer);
  struct event_details ev = ev_user_msg;
  ev.payload_size = 2 + len;
  p = append_event(&ev);
  p = write_16(p, len);
  p = write_string(p, buffer);
  va_end(args);
}

void caml_teardown_eventlog()
{
  caml_plat_lock(&lock);
  if (output) {
    num_users--;
    if (evbuf.buffer) {
      caml_ev_pause(EV_PAUSE_TERMINATE);
      flush_eventlog();
      free(evbuf.buffer);
      evbuf.buffer = NULL;
    }
    if (num_users == 0) {
      fputc(0xff, output);
      fputc(0xff, output);
      fclose(output);
      output = 0;
    }
  }
  caml_plat_unlock(&lock);
}
