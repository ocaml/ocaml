#include <stdio.h>
#include <string.h>
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

struct event_details {
  EventType type;
  int payload_size;
  const char* desc;
};

const struct event_details event_details[] = {
  { EVENT_GC_START, 0, "Starting GC" },
  { EVENT_GC_END, 0, "Finished GC" },
  { EVENT_BLOCK_MARKER, 4 + 8 + 2, "Block marker" },
};

#define NUM_EVENTS (sizeof(event_details) / sizeof(event_details[0]))

static int event_size[EVENT_MAX];

static void init_event_sizes() {
  int i;
  for (i = 0; i < EVENT_MAX; i++) {
    event_size[i] = -1;
  }
  for (i = 0; i < NUM_EVENTS; i++) {
    event_size[event_details[i].type] =
        2 /* event type */
      + 8 /* timestamp */
      + event_details[i].payload_size;
  }
}

static int get_event_size(EventType ev)
{
  if (ev == EVENT_END) return 10;
  else return event_size[ev];
}


static FILE* output;
static uint64 initial_timestamp;

#define EVENT_BUFFER_SIZE 8192
struct event_buffer {
  char* buffer;
  char* pos;
  mlsize_t remaining;
};

static __thread struct event_buffer evbuf;


static void output_event_descriptions();


uint64 timestamp()
{
  uint64 now = 0;
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

void caml_setup_eventlog() {
  char filename[200 + sizeof(".eventlog")];
#if !defined(HAS_GETTIMEOFDAY)
  caml_gc_log("No gettimeofday() on this system, event logging disabled");
#endif

  sprintf(filename, "%.200s.eventlog", caml_params->exe_name);
  output = fopen(filename, "w");
  if (!output) {
    caml_gc_log("Failed to open event log '%s': %s", filename,
                strerror(errno));
    return;
  }
  caml_gc_log("Logging events to '%s'", filename);
  initial_timestamp = timestamp();
  init_event_sizes();
  output_event_descriptions();
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

static char* write_32(char* p, uint32 x)
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
  uint64 now_ns = timestamp() - initial_timestamp;
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
}

static void output_event_descriptions()
{
  int i;
  char* buf = malloc(EVENT_BUFFER_SIZE);
  char* p = buf;

  p = write_string(p, "hdrb"); /* HEADER_BEGIN */
  p = write_string(p, "hetb"); /* HET_BEGIN */
  for (i = 0; i < NUM_EVENTS; i++) {
    struct event_details ev = event_details[i];
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

void caml_flush_eventlog()
{
  caml_gc_log("Flushing event log");
  if (evbuf.buffer && evbuf.remaining < EVENT_BUFFER_SIZE) {
    uint32 len = (uint32)(evbuf.pos - evbuf.buffer);
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

static char* caml_append_data(int size) {
  char* p;
  if (size > evbuf.remaining) {
    caml_flush_eventlog();
    if (!evbuf.buffer)
      evbuf.buffer = malloc(EVENT_BUFFER_SIZE);
    /* add block header */
    p = evbuf.buffer;
    p = write_16(p, EVENT_BLOCK_MARKER);
    p = write_timestamp(p);
    p = write_32(p, 0);     /* overwritten later */
    p = write_timestamp(p); /* overwritten later */
    p = write_16(p, caml_domain_self()->id);
    evbuf.pos = p;
    evbuf.remaining = EVENT_BUFFER_SIZE - (p - evbuf.buffer);
  }
  Assert(evbuf.remaining + evbuf.pos == evbuf.buffer + EVENT_BUFFER_SIZE);
  p = evbuf.pos;
  evbuf.pos += size;
  evbuf.remaining -= size;
  return p;
}

static char* caml_append_event(EventType ev)
{
  char* p = caml_append_data(get_event_size(ev));
  p = write_16(p, ev);
  p = write_timestamp(p);
  return p;
}

void caml_log_event(EventType ev)
{
  if (!output) return;
  caml_append_event(ev);
}

void caml_teardown_eventlog()
{
  if (output) {
    /*char* p = caml_append_data(2);
      p = write_16(p, EVENT_END); */
    caml_flush_eventlog();
    fputc(0xff, output);
    fputc(0xff, output);
    fclose(output);
    output = 0;
  }
}
