#include "mlvalues.h"
#include "memory.h"
#include "callback.h"

value mycallback1(value fun, value arg)
{
  value res;
  res = callback(fun, arg);
  return res;
}

value mycallback2(value fun, value arg1, value arg2)
{
  value res;
  res = callback2(fun, arg1, arg2);
  return res;
}

value mycallback3(value fun, value arg1, value arg2, value arg3)
{
  value res;
  res = callback3(fun, arg1, arg2, arg3);
  return res;
}

value mycallback4(value fun, value arg1, value arg2, value arg3, value arg4)
{
  value args[4];
  value res;
  args[0] = arg1;
  args[1] = arg2;
  args[2] = arg3;
  args[3] = arg4;
  res = callbackN(fun, 4, args);
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
