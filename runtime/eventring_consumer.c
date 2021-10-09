/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*                          Sadiq Jaffer, Opsian                          */
/*                                                                        */
/*   Copyright 2021 Opsian Ltd                                            */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#define CAML_INTERNALS

#include "caml/alloc.h"
#include "caml/callback.h"
#include "caml/custom.h"
#include "caml/eventring.h"
#include "caml/fail.h"
#include "caml/memory.h"
#include "caml/misc.h"
#include "caml/mlvalues.h"
#include "caml/osdeps.h"

#include <fcntl.h>
#include <stdatomic.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/stat.h>

#ifdef _WIN32
#include <process.h>
#include <wtypes.h>
#elif defined(HAS_UNISTD)
#include <unistd.h>
#endif

#define RING_FILE_NAME_MAX_LEN 512

struct caml_eventring_cursor {
  int cursor_open;                  /* has this cursor been opened? */
  struct eventring_metadata_header *metadata; /* pointer to the ring metadata */
  uint64_t *current_positions;      /* positions in the rings for each domain */
  size_t ring_file_size_bytes; /* the size of the eventring file in bytes */
  int next_read_domain;        /* the last domain we read from */
  /* callbacks */
  int (*runtime_begin)(int domain_id, void *callback_data, uint64_t timestamp,
                        ev_runtime_phase phase);
  int (*runtime_end)(int domain_id, void *callback_data, uint64_t timestamp,
                      ev_runtime_phase phase);
  int (*runtime_counter)(int domain_id, void *callback_data,
                          uint64_t timestamp, ev_runtime_counter counter,
                          uint64_t val);
  int (*alloc)(int domain_id, void *callback_data, uint64_t timestamp,
                uint64_t *sz);
  int (*lifecycle)(int domain_id, void *callback_data, int64_t timestamp,
                    ev_lifecycle lifecycle, int64_t data);
  int (*lost_events)(int domain_id, void *callback_data, int lost_words);
};

/* C-API for reading from an eventring */

eventring_error
caml_eventring_create_cursor(const char *eventring_path, int pid,
                             struct caml_eventring_cursor **cursor_res) {
  int ring_fd, ret;
  struct stat tmp_stat;

  struct caml_eventring_cursor *cursor =
      caml_stat_alloc_noexc(sizeof(struct caml_eventring_cursor));
  char *eventring_loc;

  if (cursor == NULL) {
    return E_ALLOC_FAIL;
  }

  eventring_loc = caml_stat_alloc_noexc(RING_FILE_NAME_MAX_LEN);

  if (eventring_loc == NULL) {
    return E_ALLOC_FAIL;
  }

  /* If pid < 0 then we create a cursor for the current process */
  if (pid < 0) {
    eventring_loc = caml_eventring_current_location();

    if( eventring_loc == NULL ) {
      return E_NO_CURRENT_RING;
    }
  } else {
  /* In this case we are reading the ring for a different process */
    if (eventring_path) {
      ret = snprintf_os(eventring_loc, RING_FILE_NAME_MAX_LEN,
                      T("%s/%d.eventring"), eventring_path, pid);
    } else {
      ret =
          snprintf_os(eventring_loc, RING_FILE_NAME_MAX_LEN,
                      T("%d.eventring"), pid);
    }

    if (ret < 0) {
      caml_stat_free(cursor);
      caml_stat_free(eventring_loc);
      return E_PATH_FAILURE;
    }
  }

  ring_fd = open(eventring_loc, O_RDONLY, 0);
  ret = fstat(ring_fd, &tmp_stat);

  if (ret < 0) {
    caml_stat_free(cursor);
    caml_stat_free(eventring_loc);
    return E_STAT_FAILURE;
  }

  cursor->ring_file_size_bytes = tmp_stat.st_size;
  cursor->metadata = mmap(NULL, cursor->ring_file_size_bytes, PROT_READ,
                          MAP_SHARED, ring_fd, 0);
  cursor->current_positions =
      caml_stat_alloc(cursor->metadata->max_domains * sizeof(uint64_t));
  for (int j = 0; j < cursor->metadata->max_domains; j++) {
    cursor->current_positions[j] = 0;
  }
  cursor->cursor_open = 1;
  cursor->next_read_domain = 0;

  cursor->runtime_begin = NULL;
  cursor->runtime_end = NULL;
  cursor->runtime_counter = NULL;
  cursor->alloc = NULL;
  cursor->lifecycle = NULL;
  cursor->lost_events = NULL;

  *cursor_res = cursor;

  return E_SUCCESS;
}

void caml_eventring_set_runtime_begin(struct caml_eventring_cursor *cursor,
                                      int (*f)(int domain_id,
                                               void *callback_data,
                                               uint64_t timestamp,
                                               ev_runtime_phase phase)) {
  cursor->runtime_begin = f;
}

void caml_eventring_set_runtime_end(struct caml_eventring_cursor *cursor,
                                    int (*f)(int domain_id, void *callback_data,
                                             uint64_t timestamp,
                                             ev_runtime_phase phase)) {
  cursor->runtime_end = f;
}

void caml_eventring_set_runtime_counter(
    struct caml_eventring_cursor *cursor,
    int (*f)(int domain_id, void *callback_data, uint64_t timestamp,
             ev_runtime_counter counter, uint64_t val)) {
  cursor->runtime_counter = f;
}

void caml_eventring_set_alloc(struct caml_eventring_cursor *cursor,
                              int (*f)(int domain_id, void *callback_data,
                                       uint64_t timestamp, uint64_t *sz)) {
  cursor->alloc = f;
}

void caml_eventring_set_lifecycle(struct caml_eventring_cursor *cursor,
                                  int (*f)(int domain_id, void *callback_data,
                                            int64_t timestamp,
                                            ev_lifecycle lifecycle,
                                            int64_t data)) {
  cursor->lifecycle = f;
}

void caml_eventring_set_lost_events(struct caml_eventring_cursor *cursor,
                                    int (*f)(int domain_id,
                                              void *callback_data,
                                              int lost_words)) {
  cursor->lost_events = f;
}

/* frees a cursor obtained from caml_eventring_reader_create */
void caml_eventring_free_cursor(struct caml_eventring_cursor *cursor) {
  if (cursor->cursor_open) {
    cursor->cursor_open = 0;
    munmap(cursor->metadata, cursor->ring_file_size_bytes);
    caml_stat_free(cursor->current_positions);
    caml_stat_free(cursor);
  }
}

eventring_error
caml_eventring_read_poll(struct caml_eventring_cursor *cursor,
                         void *callback_data, uintnat max_events,
                         uintnat *events_consumed) {
  int consumed = 0;
  int start_domain = cursor->next_read_domain;
  uint64_t ring_head, ring_tail;
  int early_exit = 0;

  if (!cursor->cursor_open) {
    return E_CURSOR_NOT_OPEN;
  }

  /* this loop looks a bit odd because we're iterating from the last domain
     that we read from on the last read_poll call and then looping around. */
  for (int i = 0; i < cursor->metadata->max_domains && !early_exit; i++) {
    int domain_num = (start_domain + i) % cursor->metadata->max_domains;

    struct eventring_buffer_header *eventring_buffer_header =
        (struct eventring_buffer_header *)(
          (char*)cursor->metadata +
          cursor->metadata->headers_offset +
          domain_num * cursor->metadata->ring_header_size_bytes
        );

    uint64_t *ring_ptr = (uint64_t *)((char*)cursor->metadata +
                                      cursor->metadata->data_offset +
                                domain_num * cursor->metadata->ring_size_bytes);

    do {
      uint64_t buf[EVENTRING_MAX_MSG_LENGTH];
      uint64_t ring_mask, header, msg_length;
      ring_head = atomic_load_explicit(&eventring_buffer_header->ring_head,
                                       memory_order_acquire);
      ring_tail = atomic_load_explicit(&eventring_buffer_header->ring_tail,
                                       memory_order_acquire);

      if (ring_head > cursor->current_positions[domain_num]) {
        if (cursor->lost_events) {
          cursor->lost_events(domain_num, callback_data,
                                    ring_head -
                                        cursor->current_positions[domain_num]);
        }
        cursor->current_positions[domain_num] = ring_head;
      }

      if (cursor->current_positions[domain_num] >= ring_tail) {
        break;
      }

      ring_mask = cursor->metadata->ring_size_elements - 1;
      header = ring_ptr[cursor->current_positions[domain_num] & ring_mask];
      msg_length = EVENTRING_ITEM_LENGTH(header);

      if (msg_length > EVENTRING_MAX_MSG_LENGTH) {
        return E_CORRUPT_STREAM;
      }

      memcpy(buf,
             ring_ptr + (cursor->current_positions[domain_num] & ring_mask),
             msg_length * sizeof(uint64_t));

      ring_head = atomic_load_explicit(&eventring_buffer_header->ring_head,
                                       memory_order_acquire);

      /* Check the message we've read hasn't been overwritten by the writer */
      if (ring_head > cursor->current_positions[domain_num]) {
        /* It potentially has, retry for the next one after we've notified
             the callbacks about lost messages. */
        int lost_words = ring_head - cursor->current_positions[domain_num];
        cursor->current_positions[domain_num] = ring_head;

        if (cursor->lost_events) {
          if( !(cursor->lost_events(domain_num, callback_data, lost_words)) ) {
            early_exit = 1;
            continue;
          }

        }
      }

      switch (EVENTRING_ITEM_TYPE(header)) {
      case EV_BEGIN:
        if (cursor->runtime_begin) {
          if( !cursor->runtime_begin(domain_num, callback_data, buf[1],
                                      EVENTRING_ITEM_ID(header)) ) {
                                        early_exit = 1;
                                        continue;
                                      }
        }
        break;
      case EV_EXIT:
        if (cursor->runtime_end) {
          if( !cursor->runtime_end(domain_num, callback_data, buf[1],
                                    EVENTRING_ITEM_ID(header)) ) {
                                      early_exit = 1;
                                      continue;
                                    };
        }
        break;
      case EV_COUNTER:
        if (cursor->runtime_counter) {
          if( !cursor->runtime_counter(domain_num, callback_data, buf[1],
                                        EVENTRING_ITEM_ID(header), buf[2]) ) {
                                          early_exit = 1;
                                          continue;
                                        };
        }
        break;
      case EV_ALLOC:
        if (cursor->alloc) {
          if( !cursor->alloc(domain_num, callback_data, buf[1], &buf[2])) {
            early_exit = 1;
            continue;
          }
        }
        break;
      case EV_LIFECYCLE:
        if (cursor->lifecycle) {
          if( !cursor->lifecycle(domain_num, callback_data, buf[1],
                                  EVENTRING_ITEM_ID(header), buf[2]) ) {
                                    early_exit = 1;
                                    continue;
                                  }
        }
      }

      if (EVENTRING_ITEM_TYPE(header) != EV_INTERNAL) {
        consumed++;
      }

      cursor->current_positions[domain_num] += msg_length;
    } while (cursor->current_positions[domain_num] < ring_tail &&
             (max_events == 0 || consumed < max_events) && !early_exit);

    cursor->next_read_domain = (domain_num + 1) % cursor->metadata->max_domains;
  }

  if (events_consumed != NULL) {
    *events_consumed = consumed;
  }

  return E_SUCCESS;
}

#define Cursor_val(v) (*((struct caml_eventring_cursor **)Data_custom_val(v)))

static void finalise_cursor(value v) {
  struct caml_eventring_cursor *cursor = Cursor_val(v);

  if (cursor != NULL) {
    caml_eventring_free_cursor(cursor);

    Cursor_val(v) = NULL;
  }
}

static int ml_runtime_begin(int domain_id, void *callback_data,
                             uint64_t timestamp, ev_runtime_phase phase) {
  CAMLparam0();
  CAMLlocal4(tmp_callback, ts_val, msg_type, callbacks_root);

  callbacks_root = *((value *)callback_data);

  tmp_callback = Field(callbacks_root, 0); /* ev_runtime_begin */
  if (Is_some(tmp_callback)) {
    ts_val = caml_copy_int64(timestamp);
    msg_type = Val_long(phase);

    caml_callback3(Some_val(tmp_callback), Val_long(domain_id), ts_val,
                   msg_type);
  }

  CAMLdrop;
  return 1;
}

static int ml_runtime_end(int domain_id, void *callback_data,
                           uint64_t timestamp, ev_runtime_phase phase) {
  CAMLparam0();
  CAMLlocal4(tmp_callback, ts_val, msg_type, callbacks_root);

  callbacks_root = *((value *)callback_data);

  tmp_callback = Field(callbacks_root, 1); /* ev_runtime_end */
  if (Is_some(tmp_callback)) {
    ts_val = caml_copy_int64(timestamp);
    msg_type = Val_long(phase);

    caml_callback3(Some_val(tmp_callback), Val_long(domain_id), ts_val,
                   msg_type);
  }

  CAMLdrop;
  return 1;
}

static int ml_runtime_counter(int domain_id, void *callback_data,
                               uint64_t timestamp, ev_runtime_counter counter,
                               uint64_t val) {
  CAMLparam0();
  CAMLlocal2(tmp_callback, callbacks_root);
  CAMLlocalN(params, 4);

  callbacks_root = *((value *)callback_data);

  tmp_callback = Field(callbacks_root, 2); /* ev_runtime_counter */
  if (Is_some(tmp_callback)) {
    params[0] = Val_long(domain_id);
    params[1] = caml_copy_int64(timestamp);
    params[2] = Val_long(counter);
    params[3] = Val_long(val);

    caml_callbackN(Some_val(tmp_callback), 4, params);
  }

  CAMLdrop;
  return 1;
}

static int ml_alloc(int domain_id, void *callback_data, uint64_t timestamp,
                     uint64_t *sz) {
  CAMLparam0();
  CAMLlocal4(tmp_callback, ts_val, misc_val, callbacks_root);

  callbacks_root = *((value *)callback_data);

  tmp_callback = Field(callbacks_root, 3); /* ev_alloc */
  if (Is_some(tmp_callback)) {
    int i;

    ts_val = caml_copy_int64(timestamp);
    misc_val = caml_alloc(EVENTRING_NUM_ALLOC_BUCKETS, 0);

    for (i = 0; i < EVENTRING_NUM_ALLOC_BUCKETS; i++) {
      Store_field(misc_val, i, Val_long(sz[i]));
    }

    caml_callback3(Some_val(tmp_callback), Val_long(domain_id), ts_val,
                   misc_val);
  }

  CAMLdrop;
  return 1;
}

static int ml_lifecycle(int domain_id, void *callback_data, int64_t timestamp,
                         ev_lifecycle lifecycle, int64_t data) {
  CAMLparam0();
  CAMLlocal2(tmp_callback, callbacks_root);
  CAMLlocalN(params, 4);

  callbacks_root = *((value *)callback_data);

  tmp_callback = Field(callbacks_root, 4); /* ev_lifecycle */
  if (Is_some(tmp_callback)) {
    params[0] = Val_long(domain_id);
    params[1] = caml_copy_int64(timestamp);
    params[2] = Val_long(lifecycle);
    if (data != 0) {
      params[3] = caml_alloc(1, 0);
      Store_field(params[3], 0, Val_long(data));
    } else {
      params[3] = Val_none;
    }

    caml_callbackN(Some_val(tmp_callback), 4, params);
  }

  CAMLdrop;
  return 1;
}

static int ml_lost_events(int domain_id, void *callback_data, int lost_words) {
  CAMLparam0();
  CAMLlocal2(tmp_callback, callbacks_root);

  callbacks_root = *((value *)callback_data);

  tmp_callback = Field(callbacks_root, 5); /* lost_events */

  if (Is_some(tmp_callback)) {
    caml_callback2(Some_val(tmp_callback), Val_long(domain_id),
                   Val_long(lost_words));
  }

  CAMLdrop;
  return 1;
}

static struct custom_operations cursor_operations = {
    "eventring.cursor",         finalise_cursor,
    custom_compare_default,     custom_hash_default,
    custom_serialize_default,   custom_deserialize_default,
    custom_compare_ext_default, custom_fixed_length_default};

CAMLprim value caml_ml_eventring_create_cursor(value path_pid_option) {
  CAMLparam1(path_pid_option);
  CAMLlocal1(wrapper);
  struct caml_eventring_cursor *cursor;
  int pid;
  const char *path;
  eventring_error res;

  wrapper = caml_alloc_custom(&cursor_operations,
                              sizeof(struct caml_eventring_cursor *), 0, 1);

  Cursor_val(wrapper) = NULL;

  if (Is_some(path_pid_option)) {
    path = String_val(Field(path_pid_option, 0));
    pid = Int_val(Field(path_pid_option, 1));
  } else {
    path = NULL;
    pid = -1;
  }

  res = caml_eventring_create_cursor(path, pid, &cursor);

  if (res != E_SUCCESS) {
    switch(res) {
      case E_PATH_FAILURE:
        caml_failwith("Could not construct path for cursor");
      case E_STAT_FAILURE:
        caml_failwith(
          "Could create cursor for specified path. Was eventring started?");
      case E_NO_CURRENT_RING:
        caml_failwith("No ring for current process. Was eventring started?");
      default:
        caml_failwith("Could not obtain cursor");
    }
  }

  caml_eventring_set_runtime_begin(cursor, ml_runtime_begin);
  caml_eventring_set_runtime_end(cursor, ml_runtime_end);
  caml_eventring_set_runtime_counter(cursor, ml_runtime_counter);
  caml_eventring_set_alloc(cursor, ml_alloc);
  caml_eventring_set_lifecycle(cursor, ml_lifecycle);
  caml_eventring_set_lost_events(cursor, ml_lost_events);

  Cursor_val(wrapper) = cursor;

  CAMLreturn(wrapper);
}

CAMLprim value caml_ml_eventring_free_cursor(value wrapped_cursor) {
  CAMLparam1(wrapped_cursor);

  struct caml_eventring_cursor *cursor = Cursor_val(wrapped_cursor);

  if (cursor != NULL) {
    caml_eventring_free_cursor(cursor);
    Cursor_val(wrapped_cursor) = NULL;
  }

  CAMLreturn(Val_unit);
}

CAMLprim value caml_ml_eventring_read_poll(value wrapped_cursor,
                                                value callbacks_val,
                                                value max_events_val) {
  CAMLparam3(wrapped_cursor, callbacks_val, max_events_val);

  uintnat events_consumed = 0;
  int max_events = Is_some(max_events_val) ? Some_val(max_events_val) : 0;
  struct caml_eventring_cursor *cursor = Cursor_val(wrapped_cursor);
  eventring_error res;

  if (cursor == NULL) {
    caml_failwith("Invalid or closed cursor");
  }

  if (!cursor->cursor_open) {
    caml_failwith("Eventring cursor is not open");
  }

  res = caml_eventring_read_poll
                        (cursor, &callbacks_val, max_events, &events_consumed);

  if (res != E_SUCCESS) {
    switch (res) {
    case E_CORRUPT_STREAM:
      caml_failwith("corrupt stream");
    case E_CURSOR_NOT_OPEN:
      caml_failwith("cursor is not open");
    default:
      /* this should never happen */
      caml_failwith("unspecified error");
    }
  }

  CAMLreturn(Int_val(events_consumed));
};
