#include "test4.h"

int funcA(int x)
{
  if (x <= 0)
  {
    return 0;
  }
  return funcB(x - 1);
}