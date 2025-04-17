#include "test4.h"

int funcB(int x)
{
  if (x <= 0)
  {
    return 1;
  }
  return funcA(x - 1);
}

int main()
{
  __assert__(funcA(5) == 1);
  return 0;
}