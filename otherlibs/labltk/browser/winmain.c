#include <callback.h>
#include <windows.h>
extern int __argc;
extern char **__argv;
extern void expand_command_line (int *, char ***);
extern void caml_main (char **);

int WINAPI WinMain(HINSTANCE h, HINSTANCE HPrevInstance,
                   LPSTR lpCmdLine, int nCmdShow)
{
  return main(__argc, __argv);
}
