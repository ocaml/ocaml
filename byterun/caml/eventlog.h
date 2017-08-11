#ifndef CAML_EVENTLOG_H
#define CAML_EVENTLOG_H
#include "domain.h"

void caml_setup_eventlog();
void caml_teardown_eventlog();


void caml_ev_start_gc();
void caml_ev_end_gc();
void caml_ev_request_stw();


#define EV_PAUSE_BLOCK -1
#define EV_PAUSE_GC -2
#define EV_PAUSE_TERMINATE -3
#define EV_PAUSE_YIELD -4
#define EV_PAUSE_RPC(domain) (domain)
void caml_ev_pause(long reason);
void caml_ev_resume();
void caml_ev_wakeup(struct domain* domain);

void caml_ev_msg(const char* msg);

#endif
