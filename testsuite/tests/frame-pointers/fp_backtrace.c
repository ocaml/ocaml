#include <execinfo.h>
#include <unistd.h>
#include <setjmp.h>
#include <signal.h>
#include <stdio.h>

#define ARRSIZE(a)  (sizeof(a) / sizeof(*(a)))

typedef struct frame_info
{
  struct frame_info*  prev;     /* rbp */
  void*               retaddr;  /* rip */
} frame_info;

jmp_buf resume_buf;


static void signal_handler(int signum)
{
  /* Should be safe to be called from a signal handler.
   * See 21.2.1 "Performing a nonlocal goto from a signal handler" from
   * The Linux Programming Interface, Michael Kerrisk */
  siglongjmp(resume_buf, 1);
}

static int install_signal_handlers(const int signals[], struct sigaction
    handlers[], int count)
{
  for (int i = 0; i < count; i++) {
    struct sigaction action = { 0 };
    action.sa_handler = signal_handler;
    sigemptyset(&action.sa_mask);
    action.sa_flags = 0;

    if (sigaction(signals[i], &action, &handlers[i]) != 0) {
      perror("sigaction");
      return -1;
    }
  }
  return 0;
}

static int restore_signal_handlers(const int signals[], struct sigaction
    handlers[], int count)
{
  for (int i = 0; i < count; i++) {
    if (sigaction(signals[i], &handlers[i], NULL) != 0) {
      perror("sigaction");
      return -1;
    }
  }
  return 0;
}

static int safe_read(const struct frame_info* fi, struct frame_info** prev,
    void** retaddr)
{
  /* Signals to ignore while attempting to read frame_info members */
  const int signals[] = { SIGSEGV, SIGBUS };
  /* Store original signal handers */
  struct sigaction handlers[ARRSIZE(signals)] = { 0 };
  int ret = 0;

  if (install_signal_handlers(signals, handlers, ARRSIZE(signals)) != 0)
    return -1;

  if (!sigsetjmp(resume_buf, 1)) {
    *prev = fi->prev;
    *retaddr = fi->retaddr;
  } else {
    ret = -1;
  }

  if (restore_signal_handlers(signals, handlers, ARRSIZE(signals)) != 0)
    return -1;

  return ret;
}

static void print_location(void* addr)
{
  if (!addr)
    return;

  /* This requires the binary to be linked with '-rdynamic' */
  backtrace_symbols_fd(&addr, 1, STDOUT_FILENO);
}

void fp_backtrace(void)
{
  struct frame_info *fi;
  struct frame_info* next;
  void* retaddr;

  fi = __builtin_frame_address(0);
  retaddr = __builtin_extract_return_addr(__builtin_return_address(0));

  for (; fi; fi = next) {
    if (safe_read(fi, &next, &retaddr) != 0)
      return;

    print_location(retaddr);

    /* Detect the simplest kind of infinite loop */
    if (fi == next) {
      printf("fp_backtrace: loop detected\n");
      return;
    }
  }
}
