#include <windows.h>
#include <mlvalues.h>
#include <callback.h>
#include <sys.h>

/*CAMLextern int __argc; */
/* CAMLextern char **__argv; */
/* CAMLextern void caml_expand_command_line(int * argcp, char *** argvp); */
/* extern void caml_main (char **); */

int WINAPI WinMain(HINSTANCE h, HINSTANCE HPrevInstance,
                   LPSTR lpCmdLine, int nCmdShow)
{
  char exe_name[1024];
  char * argv[2];

  GetModuleFileName(NULL, exe_name, sizeof(exe_name) - 1);
  exe_name[sizeof(exe_name) - 1] = '0';
  argv[0] = exe_name;
  argv[1] = NULL;
  caml_main(argv);
  sys_exit(Val_int(0));
  return 0;
}
