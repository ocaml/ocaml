#include "mlvalues.h"
#include "memory.h"
#include "callback.h"

value mycallback1(value fun, value arg)
{
  value res;
  Push_roots(r, 2);
  r[0] = fun;
  r[1] = arg;
  res = callback(fun, arg);
  Pop_roots();
  return res;
}

value mycallback2(value fun, value arg1, value arg2)
{
  value res;
  Push_roots(r, 3);
  r[0] = fun;
  r[1] = arg1;
  r[2] = arg2;
  res = callback2(fun, arg1, arg2);
  Pop_roots();
  return res;
}

value mycallback3(value fun, value arg1, value arg2, value arg3)
{
  value res;
  Push_roots(r, 4);
  r[0] = fun;
  r[1] = arg1;
  r[2] = arg2;
  r[3] = arg3;
  res = callback3(fun, arg1, arg2, arg3);
  Pop_roots();
  return res;
}

value mypushroot(value v, value fun, value arg)
{
  Push_roots(r, 1);
  r[0] = v;
  callback(fun, arg);
  v = r[0];
  Pop_roots();
  return v;
}
