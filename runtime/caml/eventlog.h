#ifndef CAML_EVENTLOG_H
#define CAML_EVENTLOG_H
#include "domain.h"

void caml_setup_eventlog();

void caml_ev_begin(const char* name);
void caml_ev_end(const char* name);
void caml_ev_counter(const char* name, uint64_t val);
void caml_ev_begin_flow(const char* name, uintnat id);
void caml_ev_end_flow(const char* name, uintnat id);
void caml_ev_global_sync(void);
void caml_ev_tag_self_as_backup_thread(void);


/* FIXME: blocking/resuming not currently traced */
#define EV_PAUSE_BLOCK -1
#define EV_PAUSE_GC -2
#define EV_PAUSE_TERMINATE -3
#define EV_PAUSE_YIELD -4
#define EV_PAUSE_RPC(domain) (domain)
void caml_ev_pause(long reason);
void caml_ev_resume();
void caml_ev_wakeup(struct domain* domain);

#endif
