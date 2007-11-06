#include <windows.h>
#include <mlvalues.h>
#include <callback.h>
#include <sys.h>

CAMLextern int __argc;
CAMLextern char **__argv;
CAMLextern void caml_expand_command_line(int * argcp, char *** argvp);
/* extern void caml_main (char **); */

int WINAPI WinMain(HINSTANCE h, HINSTANCE HPrevInstance,
                   LPSTR lpCmdLine, int nCmdShow)
{
  caml_expand_command_line(&__argc, &__argv);
  caml_main(__argv);
  sys_exit(Val_int(0));
  return 0;
}
