#define CAML_NAME_SPACE

#include <caml/alloc.h>
#include <caml/runtime_events.h>

value get_event_max_ids(value unused)
{
  return caml_alloc_3(0,
                      Val_long(CAML_EV_RUNTIME_COUNTER_MAX),
                      Val_long(CAML_EV_RUNTIME_PHASE_MAX),
                      Val_long(CAML_EV_LIFECYCLE_MAX));
}
