/* Platform-specific concurrency primitives */

#include <pthread.h>

typedef pthread_mutex_t plat_mutex;
#define plat_mutex_init(m) pthread_mutex_init(m, 0);
#define plat_mutex_lock pthread_mutex_lock
#define plat_mutex_unlock pthread_mutex_unlock

/* Better implementations of shared_stack can use CAS (+ABA protection) or LL/SC */

typedef struct shared_stack_node {
  struct shared_stack_node* next;
} shared_stack_node;

typedef struct shared_stack {
  plat_mutex lock;
  struct shared_stack_node first;
} shared_stack;

#define SHARED_STACK_INIT { PTHREAD_MUTEX_INITIALIZER, { 0 } }

void shared_stack_init(shared_stack* stk) {
  stk->first.next = 0;
  plat_mutex_init(&stk->lock);
}

void shared_stack_push(shared_stack* stk, shared_stack_node* node) {
  plat_mutex_lock(&stk->lock);
  node->next = stk->first.next;
  stk->first.next = node;
  plat_mutex_unlock(&stk->lock);
}

void* shared_stack_pop(shared_stack* stk) {
  plat_mutex_lock(&stk->lock);
  shared_stack_node* n = stk->first.next;
  if (n) {
    stk->first.next = n->next;
  }
  plat_mutex_unlock(&stk->lock);
  return n;
}
