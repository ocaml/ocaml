/*
 Core is not written when the process' current directory at crash time
 is not writable
*/

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

int main()
{
  char *p = NULL;
  if ( chdir("/") < 0) perror("chdir");
  *p = 'a';
  return 0;
}
