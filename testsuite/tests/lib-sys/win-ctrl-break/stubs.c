#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <caml/mlvalues.h>

/* Move this process to a fresh, hidden console of its own, so that the
   console control events we generate cannot reach any other process
   (the test driver, make, the user's shell...). */
value caml_test_use_private_console(value unit)
{
  HANDLE in = GetStdHandle(STD_INPUT_HANDLE);
  HANDLE out = GetStdHandle(STD_OUTPUT_HANDLE);
  HANDLE err = GetStdHandle(STD_ERROR_HANDLE);
  BOOL ok;

  FreeConsole();
  ok = AllocConsole();
  /* AllocConsole repoints the standard handles at the new console;
     keep them pointing at whatever ocamltest redirected them to. */
  SetStdHandle(STD_INPUT_HANDLE, in);
  SetStdHandle(STD_OUTPUT_HANDLE, out);
  SetStdHandle(STD_ERROR_HANDLE, err);
  if (!ok) return Val_false;
  ShowWindow(GetConsoleWindow(), SW_HIDE);
  /* "Ignore Ctrl-C" is inherited from the parent process (e.g. when
     running under sshd); clear it so that Ctrl-C is delivered. */
  SetConsoleCtrlHandler(NULL, FALSE);
  return Val_true;
}

value caml_test_generate_ctrl_event(value v_break)
{
  DWORD event = Bool_val(v_break) ? CTRL_BREAK_EVENT : CTRL_C_EVENT;
  /* Process group 0: every process attached to our (private) console. */
  return Val_bool(GenerateConsoleCtrlEvent(event, 0));
}
