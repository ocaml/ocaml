#include <windows.h>
#include <mlvalues.h>
#include <callback.h>
#include <sys.h>

extern int __argc;
extern char **__argv;
extern void caml_expand_command_line(int * argcp, char *** argvp);
extern void caml_main (char **);

int WINAPI WinMain(HINSTANCE h, HINSTANCE HPrevInstance,
                   LPSTR lpCmdLine, int nCmdShow)
{
  caml_expand_command_line(&__argc, &__argv);
  caml_main(__argv);
  sys_exit(Val_int(0));
  return 0;
}
