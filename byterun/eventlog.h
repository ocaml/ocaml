#ifndef CAML_EVENTLOG_H
#define CAML_EVENTLOG_H

void caml_setup_eventlog();
void caml_teardown_eventlog();

typedef enum  {
  EVENT_GC_START = 9,
  EVENT_GC_END = 10,

  EVENT_BLOCK_MARKER = 18,

  EVENT_END = 0xffff,

  EVENT_MAX = 100 /* some number bigger than all events */
} EventType;

void caml_log_event(EventType);

#endif
