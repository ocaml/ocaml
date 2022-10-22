/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*                          Sadiq Jaffer, Opsian                          */
/*                                                                        */
/*   Copyright 2022 Opsian Ltd                                            */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#ifndef CAML_RUNTIME_EVENTS_CONSUMER_H
#define CAML_RUNTIME_EVENTS_CONSUMER_H

/* This file contains the runtime_events consumer functionality. For producer
   functionality and functions for starting/pausing/resuming runtime events
   see `caml/runtime_events.h`. */

#include "caml/runtime_events.h"

/* Create a cursor to read events from an runtime_events. Cursors can be created
   for runtime_events in and out of process. An runtime_events may have
   multiple cursors reading from it at any point in time and a program may have
   multiple cursors open concurrently (for example if multiple consumers want
   different sets of events). To create one for the current process, pass
   [runtime_events_path] as NULL and a [pid] < 0. Otherwise
   [runtime_events_path] is a path to a directory containing the .events
   files. [pid] is the process id (or equivalent) of the startup OCaml process.
   The resulting cursor can be used with `caml_runtime_events_read_poll` to read
   events from the runtime_events ring-buffers. */
CAMLextern runtime_events_error
caml_runtime_events_create_cursor(const char_os* runtime_events_path, int pid,
                             struct caml_runtime_events_cursor **cursor_res);

/* The following functions set callbacks on the cursor to be called with
   matching events when the cursor is polled. These return an int, 1 if
   processing should continue or 0 if the cursor poll should stop at the
   current event */

/* Set the runtime_begin event callback on the cursor */
CAMLextern void caml_runtime_events_set_runtime_begin(
    struct caml_runtime_events_cursor *cursor,
    int (*f)(int domain_id, void *callback_data, uint64_t timestamp,
             ev_runtime_phase phase));

/* Set the runtime_end event callback on the cursor */
CAMLextern void caml_runtime_events_set_runtime_end(
    struct caml_runtime_events_cursor *cursor,
    int (*f)(int domain_id, void *callback_data, uint64_t timestamp,
             ev_runtime_phase phase));

/* Set the runtime_counter event callback on the cursor */
CAMLextern void caml_runtime_events_set_runtime_counter(
    struct caml_runtime_events_cursor *cursor,
    int (*f)(int domain_id, void *callback_data, uint64_t timestamp,
             ev_runtime_counter counter, uint64_t val));

/* Set the alloc event callback on the cursor */
CAMLextern void
caml_runtime_events_set_runtime_alloc(struct caml_runtime_events_cursor *cursor,
                                 int (*f)(int domain_id, void *callback_data,
                                          uint64_t timestamp, uint64_t *sz));

/* Set the lifecycle event callback on the cursor */
CAMLextern void caml_runtime_events_set_lifecycle(
    struct caml_runtime_events_cursor *cursor,
    int (*f)(int domain_id, void *callback_data, int64_t timestamp,
              ev_lifecycle lifecycle, int64_t data));

/* Set the lost events callback on the cursor */
CAMLextern void caml_runtime_events_set_lost_events(
    struct caml_runtime_events_cursor *cursor,
    int (*f)(int domain_id, void *callback_data, int lost_words));

/* frees a cursor obtained from caml_runtime_events_creator_cursor */
CAMLextern void
caml_runtime_events_free_cursor(struct caml_runtime_events_cursor *cursor);

/* polls the runtime_events pointed to by [cursor] and calls the appropriate
   callback for each new event up to at most [max_events] times. 0 for
   [max_events] indicates no limit to the number of callbacks. Returns the
   number of events consumed in [events_consumed], if set. [callback_data] is
   passed to any callbacks run for events. */
CAMLextern runtime_events_error caml_runtime_events_read_poll(
    struct caml_runtime_events_cursor *cursor,
    void *callback_data,
    uintnat max_events, uintnat *events_consumed);

#endif
