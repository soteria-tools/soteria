#include "test2.h"

int main()
{
  int ret = compute();
  __assert__(ret == 10);
  return 0;
}
