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
#include "caml/fail.h"
#include "caml/runtime_events.h"
#include "caml/memory.h"
#include "caml/misc.h"
#include "caml/mlvalues.h"
#include "caml/osdeps.h"

#include <fcntl.h>
#include <stdatomic.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <sys/stat.h>

#ifdef _WIN32
#include <process.h>
#include <processthreadsapi.h>
#include <wtypes.h>
#else
#include <sys/mman.h>
#endif


#if defined(HAS_UNISTD)
#include <unistd.h>
#endif

#define RING_FILE_NAME_MAX_LEN 512

struct caml_runtime_events_cursor {
  int cursor_open;                  /* has this cursor been opened? */
  atomic_uintnat cursor_in_poll;    /* cursor is inside a read_poll() */
  struct runtime_events_metadata_header *metadata; /* ptr to ring metadata */
  uint64_t *current_positions;      /* positions in the rings for each domain */
  size_t ring_file_size_bytes; /* size of the runtime_events file in bytes */
  int next_read_domain;        /* the next domain to read from */
#ifdef _WIN32
  HANDLE ring_file_handle;
  HANDLE ring_handle;
#endif
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

/* C-API for reading from an runtime_events */

runtime_events_error
caml_runtime_events_create_cursor(const char_os* runtime_events_path, int pid,
                             struct caml_runtime_events_cursor **cursor_res) {
  int ret;

#ifndef _WIN32
  int ring_fd;
  struct stat tmp_stat;
#endif

  struct caml_runtime_events_cursor *cursor =
      caml_stat_alloc_noexc(sizeof(struct caml_runtime_events_cursor));
  char_os *runtime_events_loc;

  if (cursor == NULL) {
    return E_ALLOC_FAIL;
  }

  runtime_events_loc = caml_stat_alloc_noexc(RING_FILE_NAME_MAX_LEN);

  if (runtime_events_loc == NULL) {
    caml_stat_free(cursor);
    return E_ALLOC_FAIL;
  }

  /* If pid < 0 then we create a cursor for the current process */
  if (pid < 0) {
    runtime_events_loc = caml_runtime_events_current_location();

    if( runtime_events_loc == NULL ) {
      caml_stat_free(cursor);
      return E_NO_CURRENT_RING;
    }
  } else {
  /* In this case we are reading the ring for a different process */
    if (runtime_events_path) {
      char* path_u8 = caml_stat_strdup_of_os(runtime_events_path);
      ret = snprintf_os(runtime_events_loc, RING_FILE_NAME_MAX_LEN,
                      T("%s/%d.events"), path_u8, pid);
      caml_stat_free(path_u8);
    } else {
      ret =
          snprintf_os(runtime_events_loc, RING_FILE_NAME_MAX_LEN,
                      T("%d.events"), pid);
    }

    if (ret < 0) {
      caml_stat_free(cursor);
      caml_stat_free(runtime_events_loc);
      return E_PATH_FAILURE;
    }
  }

#ifdef _WIN32
  cursor->ring_file_handle = CreateFile(
    runtime_events_loc,
    GENERIC_READ | GENERIC_WRITE,
    FILE_SHARE_READ | FILE_SHARE_WRITE,
    NULL,
    OPEN_EXISTING,
    FILE_ATTRIBUTE_NORMAL,
    NULL
  );

  if (cursor->ring_file_handle == INVALID_HANDLE_VALUE) {
    caml_stat_free(cursor);
    caml_stat_free(runtime_events_loc);
    return E_OPEN_FAILURE;
  }

  cursor->ring_handle = CreateFileMapping(
    cursor->ring_file_handle,
    NULL,
    PAGE_READWRITE,
    0,
    0,
    NULL
  );

  if (cursor->ring_handle == INVALID_HANDLE_VALUE) {
    caml_stat_free(cursor);
    caml_stat_free(runtime_events_loc);
    return E_MAP_FAILURE;
  }

  cursor->metadata = MapViewOfFile(
    cursor->ring_handle,
    FILE_MAP_ALL_ACCESS,
    0,
    0,
    0
  );

  if( cursor->metadata == NULL ) {
    caml_stat_free(cursor);
    caml_stat_free(runtime_events_loc);
    return E_MAP_FAILURE;
  }

  cursor->ring_file_size_bytes = GetFileSize(cursor->ring_file_handle, NULL);
#else
  ring_fd = open(runtime_events_loc, O_RDONLY, 0);

  if( ring_fd == -1 ) {
    caml_stat_free(cursor);
    caml_stat_free(runtime_events_loc);
    return E_OPEN_FAILURE;
  }

  ret = fstat(ring_fd, &tmp_stat);

  if (ret < 0) {
    caml_stat_free(cursor);
    caml_stat_free(runtime_events_loc);
    return E_OPEN_FAILURE;
  }

  cursor->ring_file_size_bytes = tmp_stat.st_size;

  /* This cast is necessary for compatibility with Illumos' non-POSIX
    mmap/munmap */
  cursor->metadata = (struct runtime_events_metadata_header *)
                      mmap(NULL, cursor->ring_file_size_bytes, PROT_READ,
                          MAP_SHARED, ring_fd, 0);

  if( cursor->metadata == MAP_FAILED ) {
    caml_stat_free(cursor);
    caml_stat_free(runtime_events_loc);
    return E_MAP_FAILURE;
  }
#endif

  cursor->current_positions =
      caml_stat_alloc(cursor->metadata->max_domains * sizeof(uint64_t));

  for (int j = 0; j < cursor->metadata->max_domains; j++) {
    cursor->current_positions[j] = 0;
  }
  cursor->cursor_open = 1;
  atomic_store(&cursor->cursor_in_poll, 0);
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

void caml_runtime_events_set_runtime_begin(
                                      struct caml_runtime_events_cursor *cursor,
                                      int (*f)(int domain_id,
                                               void *callback_data,
                                               uint64_t timestamp,
                                               ev_runtime_phase phase)) {
  cursor->runtime_begin = f;
}

void caml_runtime_events_set_runtime_end(
                                    struct caml_runtime_events_cursor *cursor,
                                    int (*f)(int domain_id, void *callback_data,
                                             uint64_t timestamp,
                                             ev_runtime_phase phase)) {
  cursor->runtime_end = f;
}

void caml_runtime_events_set_runtime_counter(
    struct caml_runtime_events_cursor *cursor,
    int (*f)(int domain_id, void *callback_data, uint64_t timestamp,
             ev_runtime_counter counter, uint64_t val)) {
  cursor->runtime_counter = f;
}

void caml_runtime_events_set_alloc(struct caml_runtime_events_cursor *cursor,
                              int (*f)(int domain_id, void *callback_data,
                                       uint64_t timestamp, uint64_t *sz)) {
  cursor->alloc = f;
}

void caml_runtime_events_set_lifecycle(
                                  struct caml_runtime_events_cursor *cursor,
                                  int (*f)(int domain_id, void *callback_data,
                                            int64_t timestamp,
                                            ev_lifecycle lifecycle,
                                            int64_t data)) {
  cursor->lifecycle = f;
}

void caml_runtime_events_set_lost_events(
                                    struct caml_runtime_events_cursor *cursor,
                                    int (*f)(int domain_id,
                                              void *callback_data,
                                              int lost_words)) {
  cursor->lost_events = f;
}

/* frees a cursor obtained from caml_runtime_events_reader_create */
void caml_runtime_events_free_cursor(struct caml_runtime_events_cursor *cursor){
  if (cursor->cursor_open) {
    cursor->cursor_open = 0;
#ifdef _WIN32
    UnmapViewOfFile(cursor->metadata);
    CloseHandle(cursor->ring_file_handle);
    CloseHandle(cursor->ring_handle);
#else
    /* This cast is necessary for compatibility with Illumos' non-POSIX
      mmap/munmap */
    munmap((void*)cursor->metadata, cursor->ring_file_size_bytes);
#endif
    caml_stat_free(cursor->current_positions);
    caml_stat_free(cursor);
  }
}

runtime_events_error
caml_runtime_events_read_poll(struct caml_runtime_events_cursor *cursor,
                         void *callback_data, uintnat max_events,
                         uintnat *events_consumed) {
  int consumed = 0;
  int start_domain = cursor->next_read_domain;
  uint64_t ring_head, ring_tail;
  int early_exit = 0;
  uintnat in_poll = 0;

  if (!cursor->cursor_open) {
    return E_CURSOR_NOT_OPEN;
  }

  /* prevent cursors from being polled in parallel by more than one domain and
    also reentrant callbacks */
  if (cursor->cursor_in_poll
    || !atomic_compare_exchange_strong(&cursor->cursor_in_poll, &in_poll, 1) ) {
    return E_CURSOR_POLL_BUSY;
  }

  /* this loop looks a bit odd because we're iterating from the last domain
     that we read from on the last read_poll call and then looping around.
     This is necessary because in the case where the consumer can't keep up
     with message production (i.e max_events is hit each time) it ensures that
     messages are read from all domains, rather than just the first. */
  for (int i = 0; i < cursor->metadata->max_domains && !early_exit; i++) {
    int domain_num = (start_domain + i) % cursor->metadata->max_domains;

    struct runtime_events_buffer_header *runtime_events_buffer_header =
        (struct runtime_events_buffer_header *)(
          (char*)cursor->metadata +
          cursor->metadata->headers_offset +
          domain_num * cursor->metadata->ring_header_size_bytes
        );

    uint64_t *ring_ptr = (uint64_t *)((char*)cursor->metadata +
                                      cursor->metadata->data_offset +
                                domain_num * cursor->metadata->ring_size_bytes);

    do {
      uint64_t buf[RUNTIME_EVENTS_MAX_MSG_LENGTH];
      uint64_t ring_mask, header, msg_length;
      ring_head = atomic_load_explicit(&runtime_events_buffer_header->ring_head,
                                       memory_order_acquire);
      ring_tail = atomic_load_explicit(&runtime_events_buffer_header->ring_tail,
                                       memory_order_acquire);

      if (ring_head > cursor->current_positions[domain_num]) {
        if (cursor->lost_events) {
          if (!cursor->lost_events(domain_num, callback_data,
                                   ring_head -
                                   cursor->current_positions[domain_num])){
            early_exit = 1;
            continue;
          }
        }
        cursor->current_positions[domain_num] = ring_head;
      }

      if (cursor->current_positions[domain_num] >= ring_tail) {
        break;
      }

      ring_mask = cursor->metadata->ring_size_elements - 1;
      header = ring_ptr[cursor->current_positions[domain_num] & ring_mask];
      msg_length = RUNTIME_EVENTS_ITEM_LENGTH(header);

      if (msg_length > RUNTIME_EVENTS_MAX_MSG_LENGTH) {
        atomic_store(&cursor->cursor_in_poll, 0);
        return E_CORRUPT_STREAM;
      }

      memcpy(buf,
             ring_ptr + (cursor->current_positions[domain_num] & ring_mask),
             msg_length * sizeof(uint64_t));

      atomic_thread_fence(memory_order_seq_cst);

      ring_head = atomic_load_explicit(&runtime_events_buffer_header->ring_head,
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

      switch (RUNTIME_EVENTS_ITEM_TYPE(header)) {
      case EV_BEGIN:
        if (cursor->runtime_begin) {
          if( !cursor->runtime_begin(domain_num, callback_data, buf[1],
                                      RUNTIME_EVENTS_ITEM_ID(header)) ) {
                                        early_exit = 1;
                                        continue;
                                      }
        }
        break;
      case EV_EXIT:
        if (cursor->runtime_end) {
          if( !cursor->runtime_end(domain_num, callback_data, buf[1],
                                    RUNTIME_EVENTS_ITEM_ID(header)) ) {
                                      early_exit = 1;
                                      continue;
                                    };
        }
        break;
      case EV_COUNTER:
        if (cursor->runtime_counter) {
          if( !cursor->runtime_counter(domain_num, callback_data, buf[1],
                                       RUNTIME_EVENTS_ITEM_ID(header), buf[2]
                                      ) ) {
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
                                  RUNTIME_EVENTS_ITEM_ID(header), buf[2]) ) {
                                    early_exit = 1;
                                    continue;
                                  }
        }
      }

      if (RUNTIME_EVENTS_ITEM_TYPE(header) != EV_INTERNAL) {
        consumed++;
      }

      cursor->current_positions[domain_num] += msg_length;
    } while (cursor->current_positions[domain_num] < ring_tail &&
             (max_events == 0 || consumed < max_events) && !early_exit);

    /* next domain to read from (saved in the cursor so we can resume from it
       if need be in the next poll of the cursor). */
    cursor->next_read_domain =
      (domain_num + 1 == cursor->metadata->max_domains) ? 0 : domain_num + 1;
  }

  if (events_consumed != NULL) {
    *events_consumed = consumed;
  }

  atomic_store(&cursor->cursor_in_poll, 0);
  return E_SUCCESS;
}

/* OCaml for reading from an runtime_events */

#define Cursor_val(v) \
  (*((struct caml_runtime_events_cursor **)Data_custom_val(v)))

static void finalise_cursor(value v) {
  struct caml_runtime_events_cursor *cursor = Cursor_val(v);

  if (cursor != NULL) {
    caml_runtime_events_free_cursor(cursor);

    Cursor_val(v) = NULL;
  }
}

/* Used for passing the collection of callbacks and also storing raised
   exceptions */
struct callbacks_exception_holder {
  value* callbacks_val;
  value* exception;
};

static int ml_runtime_begin(int domain_id, void *callback_data,
                             uint64_t timestamp, ev_runtime_phase phase) {
  CAMLparam0();
  CAMLlocal5(tmp_callback, ts_val, msg_type, callbacks_root, res);
  struct callbacks_exception_holder* holder = callback_data;

  callbacks_root = *holder->callbacks_val;

  tmp_callback = Field(callbacks_root, 0); /* ev_runtime_begin */
  if (Is_some(tmp_callback)) {
    ts_val = caml_copy_int64(timestamp);
    msg_type = Val_long(phase);

    res = caml_callback3_exn(Some_val(tmp_callback), Val_long(domain_id),
                                  ts_val, msg_type);

    if( Is_exception_result(res) ) {
      res = Extract_exception(res);
      *holder->exception = res;
      CAMLdrop;
      return 0;
    }
  }

  CAMLdrop;
  return 1;
}

static int ml_runtime_end(int domain_id, void *callback_data,
                           uint64_t timestamp, ev_runtime_phase phase) {
  CAMLparam0();
  CAMLlocal5(tmp_callback, ts_val, msg_type, callbacks_root, res);
  struct callbacks_exception_holder* holder = callback_data;

  callbacks_root = *holder->callbacks_val;

  tmp_callback = Field(callbacks_root, 1); /* ev_runtime_end */
  if (Is_some(tmp_callback)) {
    ts_val = caml_copy_int64(timestamp);
    msg_type = Val_long(phase);

    res = caml_callback3_exn(Some_val(tmp_callback), Val_long(domain_id),
                             ts_val, msg_type);

    if( Is_exception_result(res) ) {
      res = Extract_exception(res);
      *holder->exception = res;
      CAMLdrop;
      return 0;
    }
  }

  CAMLdrop;
  return 1;
}

static int ml_runtime_counter(int domain_id, void *callback_data,
                               uint64_t timestamp, ev_runtime_counter counter,
                               uint64_t val) {
  CAMLparam0();
  CAMLlocal3(tmp_callback, callbacks_root, res);
  CAMLlocalN(params, 4);

  struct callbacks_exception_holder* holder = callback_data;
  callbacks_root = *holder->callbacks_val;

  tmp_callback = Field(callbacks_root, 2); /* ev_runtime_counter */
  if (Is_some(tmp_callback)) {
    params[0] = Val_long(domain_id);
    params[1] = caml_copy_int64(timestamp);
    params[2] = Val_long(counter);
    params[3] = Val_long(val);

    res = caml_callbackN_exn(Some_val(tmp_callback), 4, params);

    if( Is_exception_result(res) ) {
      res = Extract_exception(res);
      *holder->exception = res;
      CAMLdrop;
      return 0;
    }
  }

  CAMLdrop;
  return 1;
}

static int ml_alloc(int domain_id, void *callback_data, uint64_t timestamp,
                     uint64_t *sz) {
  CAMLparam0();
  CAMLlocal5(tmp_callback, ts_val, misc_val, callbacks_root, res);

  struct callbacks_exception_holder* holder = callback_data;
  callbacks_root = *holder->callbacks_val;

  tmp_callback = Field(callbacks_root, 3); /* ev_alloc */
  if (Is_some(tmp_callback)) {
    int i;

    ts_val = caml_copy_int64(timestamp);
    misc_val = caml_alloc(RUNTIME_EVENTS_NUM_ALLOC_BUCKETS, 0);

    for (i = 0; i < RUNTIME_EVENTS_NUM_ALLOC_BUCKETS; i++) {
      Store_field(misc_val, i, Val_long(sz[i]));
    }

    res = caml_callback3_exn(Some_val(tmp_callback), Val_long(domain_id),
                             ts_val, misc_val);

    if( Is_exception_result(res) ) {
      res = Extract_exception(res);
      *holder->exception = res;
      CAMLdrop;
      return 0;
    }
  }

  CAMLdrop;
  return 1;
}

static int ml_lifecycle(int domain_id, void *callback_data, int64_t timestamp,
                         ev_lifecycle lifecycle, int64_t data) {
  CAMLparam0();
  CAMLlocal3(tmp_callback, callbacks_root, res);
  CAMLlocalN(params, 4);

  struct callbacks_exception_holder* holder = callback_data;
  callbacks_root = *holder->callbacks_val;

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

    res = caml_callbackN_exn(Some_val(tmp_callback), 4, params);

    if( Is_exception_result(res) ) {
      res = Extract_exception(res);
      *holder->exception = res;
      CAMLdrop;
      return 0;
    }
  }

  CAMLdrop;
  return 1;
}

static int ml_lost_events(int domain_id, void *callback_data, int lost_words) {
  CAMLparam0();
  CAMLlocal3(tmp_callback, callbacks_root, res);

  struct callbacks_exception_holder* holder = callback_data;
  callbacks_root = *holder->callbacks_val;

  tmp_callback = Field(callbacks_root, 5); /* lost_events */

  if (Is_some(tmp_callback)) {
    res = caml_callback2_exn(Some_val(tmp_callback), Val_long(domain_id),
                   Val_long(lost_words));

    if( Is_exception_result(res) ) {
      res = Extract_exception(res);
      *holder->exception = res;
      CAMLdrop;
      return 0;
    }
  }

  CAMLdrop;
  return 1;
}

static struct custom_operations cursor_operations = {
    "runtime_events.cursor",         finalise_cursor,
    custom_compare_default,     custom_hash_default,
    custom_serialize_default,   custom_deserialize_default,
    custom_compare_ext_default, custom_fixed_length_default};

CAMLprim value caml_ml_runtime_events_create_cursor(value path_pid_option) {
  CAMLparam1(path_pid_option);
  CAMLlocal1(wrapper);
  struct caml_runtime_events_cursor *cursor;
  int pid;
  char_os* path;
  runtime_events_error res;

  wrapper = caml_alloc_custom(&cursor_operations,
                            sizeof(struct caml_runtime_events_cursor *), 0, 1);

  Cursor_val(wrapper) = NULL;

  if (Is_some(path_pid_option)) {
    const char* path_u8 = String_val(Field(Some_val(path_pid_option), 0));
    path = caml_stat_strdup_to_os(path_u8);
    pid = Int_val(Field(Some_val(path_pid_option), 1));
  } else {
    path = NULL;
    pid = -1;
  }

  res = caml_runtime_events_create_cursor(path, pid, &cursor);

  if (res != E_SUCCESS) {
    if( path != NULL ) {
      caml_stat_free(path);
    }

    switch(res) {
      case E_PATH_FAILURE:
        caml_failwith(
          "Runtime_events: could not construct path for cursor.");
      case E_OPEN_FAILURE:
        caml_failwith(
          "Runtime_events: could not create cursor for specified path.");
      case E_MAP_FAILURE:
        caml_failwith("Runtime_events: could not map underlying runtime_events."
        );
      case E_NO_CURRENT_RING:
        caml_failwith(
        "Runtime_events: no ring for current process. \
         Was runtime_events started?");
      default:
        caml_failwith("Runtime_events: could not obtain cursor");
    }
  }

  caml_runtime_events_set_runtime_begin(cursor, ml_runtime_begin);
  caml_runtime_events_set_runtime_end(cursor, ml_runtime_end);
  caml_runtime_events_set_runtime_counter(cursor, ml_runtime_counter);
  caml_runtime_events_set_alloc(cursor, ml_alloc);
  caml_runtime_events_set_lifecycle(cursor, ml_lifecycle);
  caml_runtime_events_set_lost_events(cursor, ml_lost_events);

  Cursor_val(wrapper) = cursor;

  if( path != NULL ) {
    caml_stat_free(path);
  }

  CAMLreturn(wrapper);
}

CAMLprim value caml_ml_runtime_events_free_cursor(value wrapped_cursor) {
  CAMLparam1(wrapped_cursor);

  struct caml_runtime_events_cursor *cursor = Cursor_val(wrapped_cursor);

  if (cursor != NULL) {
    caml_runtime_events_free_cursor(cursor);
    Cursor_val(wrapped_cursor) = NULL;
  }

  CAMLreturn(Val_unit);
}

CAMLprim value caml_ml_runtime_events_read_poll(value wrapped_cursor,
                                                value callbacks_val,
                                                value max_events_val) {
  CAMLparam3(wrapped_cursor, callbacks_val, max_events_val);
  CAMLlocal1(exception);

  uintnat events_consumed = 0;
  int max_events = Is_some(max_events_val) ? Some_val(max_events_val) : 0;
  struct caml_runtime_events_cursor *cursor = Cursor_val(wrapped_cursor);
  runtime_events_error res;

  struct callbacks_exception_holder holder = { &callbacks_val, &exception };
  exception = Val_unit;

  if (cursor == NULL) {
    caml_failwith("Runtime_events: invalid or closed cursor");
  }

  if (!cursor->cursor_open) {
    caml_failwith("Runtime_events: cursor is not open");
  }

  res = caml_runtime_events_read_poll
                        (cursor, &holder, max_events, &events_consumed);

  /* Check if we early exited with an exception */
  if( exception != Val_unit ) {
    caml_raise(exception);
  }

  if (res != E_SUCCESS) {
    switch (res) {
    case E_CURSOR_POLL_BUSY:
      caml_failwith("Runtime_events: poll called concurrently or reentrant");
    case E_CORRUPT_STREAM:
      caml_failwith("Runtime_events: corrupt stream");
    case E_CURSOR_NOT_OPEN:
      caml_failwith("Runtime_events: cursor is not open");
    default:
      /* this should never happen */
      caml_failwith("Runtime_events: unspecified error");
    }
  }

  CAMLreturn(Int_val(events_consumed));
};
