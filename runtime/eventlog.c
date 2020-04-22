#define CAML_INTERNALS
#include <stdio.h>
#include <string.h>
#include <stdint.h>
#include <unistd.h>
#include "caml/domain.h"
#include "caml/eventlog.h"
#include "caml/osdeps.h"
#include "caml/platform.h"
#include "caml/startup_aux.h"

#define Pi64 "%" ARCH_INT64_PRINTF_FORMAT "d"
#define Pinat "%" ARCH_INTNAT_PRINTF_FORMAT "d"
#define Pu64 "%" ARCH_INT64_PRINTF_FORMAT "u"
#define Punat "%" ARCH_INTNAT_PRINTF_FORMAT "u"

/* Using these vs int64_t/uint64_t needed to prevent warnings on OS X/clang */
#define Pi64_t ARCH_INT64_TYPE
#define Pu64_t ARCH_UINT64_TYPE

typedef enum { BEGIN, END, BEGIN_FLOW, END_FLOW, GLOBAL_SYNC, COUNTER } evtype;

struct event {
  const char* name;
  Pi64_t timestamp;
  evtype ty;
  Pu64_t value; /* for COUNTER */
};

/* events are stored thread-locally in event_buffers,
   which are registered in a global doubly-linked list,
   so that they can all be flushed on exit() */
struct event_buffer;
struct evbuf_list_node {
  struct evbuf_list_node* next;
  struct evbuf_list_node* prev;
};

/* all access to the global list, and all flushing of events
   to the output file, is done while holding this lock */
static caml_plat_mutex lock = CAML_PLAT_MUTEX_INITIALIZER;
static struct evbuf_list_node evbuf_head =
  { &evbuf_head, &evbuf_head };
static FILE* output;
static Pi64_t startup_timestamp;

#define EVENT_BUF_SIZE 32768
struct event_buffer {
  struct evbuf_list_node list;
  uintnat ev_flushed;
  int domain_unique_id;
  atomic_uintnat ev_generated;
  struct event events[EVENT_BUF_SIZE];
};

static __thread struct event_buffer* evbuf;
static pthread_key_t evbuf_pkey; // same as evbuf, used for destructor

static void thread_setup_evbuf()
{
  CAMLassert(!evbuf);
  evbuf = malloc(sizeof(*evbuf));
  if (!evbuf) return;
  pthread_setspecific(evbuf_pkey, evbuf);

  evbuf->ev_flushed = 0;
  atomic_store_rel(&evbuf->ev_generated, 0);
  evbuf->domain_unique_id = Caml_state->unique_id;

  /* add to global list */
  caml_plat_lock(&lock);
  evbuf->list.next = evbuf_head.next;
  evbuf_head.next = &evbuf->list;
  evbuf->list.prev = &evbuf_head;
  evbuf->list.next->prev = &evbuf->list;
  caml_plat_unlock(&lock);

}

static void flush_events(FILE* out, struct event_buffer* eb);
static void thread_teardown_evbuf(void* p)
{
  CAMLassert(p == evbuf);
  if(!evbuf) return;
  caml_plat_lock(&lock);
  flush_events(output, evbuf);
  /* remove from global list */
  evbuf->list.next->prev = evbuf->list.prev;
  evbuf->list.prev->next = evbuf->list.next;
  caml_plat_unlock(&lock);
  free(evbuf);
  evbuf = NULL;
}

static void teardown_eventlog(void);
void caml_setup_eventlog()
{
  char filename[64];
  if (!caml_params->eventlog_enabled) return;
  sprintf(filename, "eventlog."Pu64".json", (Pu64_t)getpid());
  caml_plat_lock(&lock);
  if (pthread_key_create(&evbuf_pkey, &thread_teardown_evbuf) == 0 &&
      (output = fopen(filename, "w"))) {
    char* fullname = realpath(filename, 0);
    fprintf(stderr, "Tracing events to %s\n", fullname);
    free(fullname);
    fprintf(output,
            "{\n"
            "\"displayTimeUnit\": \"ns\",\n"
            "\"traceEvents\": [\n");
    startup_timestamp = caml_time_counter();
  } else {
    fprintf(stderr, "Could not begin logging events to %s\n", filename);
    _exit(128);
  }
  caml_plat_unlock(&lock);
  atexit(&teardown_eventlog);
}

#define FPRINTF_EV(out, ev, id, ph, extra_fmt, ...) \
  fprintf(out, \
    "{\"ph\": \"%c\", " \
    "\"ts\": "Pi64".%03d, " \
    "\"pid\": %d, " \
    "\"tid\": %d, " \
    "\"name\": \"%s\"" \
    extra_fmt \
    "},\n", \
    (ph), \
    ((ev).timestamp - startup_timestamp) / 1000, \
    (int)(((ev).timestamp - startup_timestamp) % 1000), \
    (id), \
    (id), \
    (ev).name, \
    ## __VA_ARGS__)


/* must be called holding the lock */
static void flush_events(FILE* out, struct event_buffer* eb)
{
  uintnat i;
  uintnat n;
  n = atomic_load_acq(&eb->ev_generated);
  for (i = eb->ev_flushed; i < n; i++) {
    struct event ev = eb->events[i];
    int id = eb->domain_unique_id;
    switch (ev.ty) {
    case BEGIN:
    case END:
      FPRINTF_EV(out, ev, id,
                 ev.ty == BEGIN ? 'B' : 'E', "");
      break;
    case BEGIN_FLOW:
    case END_FLOW:
      FPRINTF_EV(out, ev, id,
                 ev.ty == BEGIN_FLOW ? 's' : 'f',
                 ", \"bp\": \"e\", \"id\": \"0x%08"ARCH_INT64_PRINTF_FORMAT"x\"",
                 ev.value);
      break;
    case GLOBAL_SYNC:
      FPRINTF_EV(out, ev, id, 'i', ", \"cat\": \"gpu\"");
      break;
    case COUNTER:
      FPRINTF_EV(out, ev, id, 'C',
                 ", \"args\": {\"value\": "Pu64"}",
                 ev.value);
      break;
    }
  }
  eb->ev_flushed = n;
}

static void teardown_eventlog()
{
  struct evbuf_list_node* b;
  int count = 0;
  /* flush all event logs */
  caml_plat_lock(&lock);
  for (b = evbuf_head.next; b != &evbuf_head; b = b->next) {
    flush_events(output, (struct event_buffer*)b);
    count++;
  }
  if (count != (evbuf ? 1 : 0))
    caml_gc_log("%d evbufs still active at exit", count);
  fprintf(output,
          "{\"name\": \"exit\", "
          "\"ph\": \"i\", "
          "\"ts\": "Pi64", "
          "\"pid\": %d, "
          "\"tid\": %d, "
          "\"s\": \"g\"}\n"
          "]\n}\n",
          ((Pi64_t)caml_time_counter() - startup_timestamp) / 1000,
          Caml_state->unique_id,
          Caml_state->unique_id);
  fclose(output);
  caml_plat_unlock(&lock);
}

static void post_event(const char* name, evtype ty, uint64_t value)
{
  uintnat i;
  struct event* ev;
  int64_t duration = 0;

  if (!caml_params->eventlog_enabled) return;
  if (!evbuf) thread_setup_evbuf();
  i = atomic_load_acq(&evbuf->ev_generated);
  CAMLassert(i <= EVENT_BUF_SIZE);
  if (i == EVENT_BUF_SIZE) {
    duration = caml_time_counter();
    caml_plat_lock(&lock);
    flush_events(output, evbuf);
    evbuf->ev_flushed = 0;
    atomic_store_rel(&evbuf->ev_generated, 0);
    caml_plat_unlock(&lock);
    duration = caml_time_counter() - duration;
    post_event("overhead#", COUNTER, duration);
    i = 1;
  }
  ev = &evbuf->events[i];
  ev->name = name;
  ev->ty = ty;
  ev->value = value;
  ev->timestamp = caml_time_counter();
  atomic_store_rel(&evbuf->ev_generated, i + 1);
}

void caml_ev_begin(const char* name)
{
  caml_gc_log("ev_begin: %s", name);
  post_event(name, BEGIN, 0);
}

void caml_ev_end(const char* name)
{
  caml_gc_log("ev_end: %s", name);
  post_event(name, END, 0);
}

void caml_ev_begin_flow(const char* name, uintnat ev)
{
  post_event(name, BEGIN_FLOW, ev);
}

void caml_ev_end_flow(const char* name, uintnat ev)
{
  post_event(name, END_FLOW, ev);
}

void caml_ev_global_sync()
{
  post_event("vblank", GLOBAL_SYNC, 0);
}

void caml_ev_counter(const char* name, uint64_t val)
{
  post_event(name, COUNTER, val);
}

void caml_ev_pause(long reason){}
void caml_ev_resume(){}
void caml_ev_wakeup(struct domain* domain){}
void caml_ev_msg(char* msg, ...){}
