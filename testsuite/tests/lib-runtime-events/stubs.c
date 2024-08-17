#define CAML_NAME_SPACE

#include <caml/alloc.h>
#include <caml/runtime_events.h>
#include <caml/runtime_events_consumer.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

#include <assert.h>

struct counters {
    int minor_started;
    int major_started;
    int minors;
    int majors;
};

void start_runtime_events() {
    caml_runtime_events_start();
}

int ev_begin(int domain_id, void* callback_data,
                uint64_t timestamp, ev_runtime_phase phase) {
    struct counters* tmp_counters = (struct counters*)callback_data;
    switch( phase ) {
        case EV_MINOR:
            tmp_counters->minor_started = 1;
            break;
        case EV_MAJOR:
            tmp_counters->major_started = 1;
            break;
        default:
            break;
    }

    return 1;
}

int ev_end(int domain_id, void* callback_data, uint64_t timestamp,
                ev_runtime_phase phase) {
    struct counters* tmp_counters = (struct counters*)callback_data;
    switch( phase ) {
        case EV_MINOR:
            assert(tmp_counters->minor_started);
            tmp_counters->minor_started = 0;
            tmp_counters->minors++;
            break;
        case EV_MAJOR:
            assert(tmp_counters->major_started);
            tmp_counters->major_started = 0;
            tmp_counters->majors++;
            break;
        default:
            break;
    }

    return 1;
}

value get_event_counts(void) {
    CAMLparam0();
    CAMLlocal1(counts_tuple);
    runtime_events_error res;
    uintnat events_consumed;

    struct counters tmp_counters = { 0 };

    counts_tuple = caml_alloc_small(2, 0);

    struct caml_runtime_events_cursor* cursor;

    res = caml_runtime_events_create_cursor(NULL, -1, &cursor);

    if( res != E_SUCCESS ) {
        caml_failwith("Runtime_events.get_event_counts: invalid or non-existent cursor");
    }

    caml_runtime_events_set_runtime_begin(cursor, &ev_begin);
    caml_runtime_events_set_runtime_end(cursor, &ev_end);

    res = caml_runtime_events_read_poll(cursor, &tmp_counters, 0,
                                   &events_consumed);

    if( res != E_SUCCESS ) {
        caml_failwith("Runtime_events.get_event_counts: error reading from rings");
    }

    Field(counts_tuple, 0) = Val_long(tmp_counters.minors);
    Field(counts_tuple, 1) = Val_long(tmp_counters.majors);

    caml_runtime_events_free_cursor(cursor);

    CAMLreturn(counts_tuple);
}
