#include <mlvalues.h>
#include <alloc.h>

extern char ** environ;

value unix_environment()
{
  return copy_string_array(environ);
}
