#include <tcl.h>
#include <tk.h>
#include <caml/mlvalues.h>
#include "camltk.h"

value camltk_tk_mainloop() /* ML */
{
  CheckInit();

  if (cltk_slave_mode)
    return Val_unit;

  if (!signal_events) {
    /* Initialise signal handling */
    signal_events = 1;
    Tk_CreateTimerHandler(100, invoke_pending_caml_signals, NULL);
  };
  Tk_MainLoop();
  return Val_unit;
}

/* Note: this HAS to be reported "as-is" in ML source */
static int event_flag_table[] = {
  TK_DONT_WAIT, TK_X_EVENTS, TK_FILE_EVENTS, TK_TIMER_EVENTS, TK_IDLE_EVENTS,
  TK_ALL_EVENTS
};

value camltk_dooneevent(flags) /* ML */
     value flags;
{
  int ret;

  CheckInit();

  ret = Tk_DoOneEvent(convert_flag_list(flags, event_flag_table));
  return Val_int(ret);
}

