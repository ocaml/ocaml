#include <stdio.h>

#ifdef _WIN32
  #include <windows.h>
#else
  #include <stdlib.h>
  #include <sys/types.h>
  #include <signal.h>
#endif

int main(int argc, char** argv)
{
#ifdef _WIN32
  DWORD pid;
  HANDLE hProcess;
#else
  pid_t pid;
#endif

  if (argc != 2) {
    printf("Usage: %s pid\n", argv[0]);
    return 1;
  }

  pid = atoi(argv[1]);
#ifdef _WIN32
  hProcess = OpenProcess(SYNCHRONIZE, FALSE, pid);

  if (!hProcess) {
    printf("Process %lu not found!\n", pid);
    return 1;
  }

  FreeConsole();

  if (!AttachConsole(pid)) {
    printf("Failed to attach to console of Process %lu\n", pid);
    CloseHandle(hProcess);
    return 1;
  }

  SetConsoleCtrlHandler(NULL, TRUE);
  GenerateConsoleCtrlEvent(0, 0);
  WaitForSingleObject(hProcess, INFINITE);
  CloseHandle(hProcess);
  FreeConsole();
#else
  if (kill(pid,SIGINT)) {
    perror("kill");
    return 1;
  }
#endif

  return 0;
}
