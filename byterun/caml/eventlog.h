#ifndef CAML_EVENTLOG_H
#define CAML_EVENTLOG_H

void caml_setup_eventlog();
void caml_teardown_eventlog();


void caml_ev_start_gc();
void caml_ev_end_gc();

#endif
