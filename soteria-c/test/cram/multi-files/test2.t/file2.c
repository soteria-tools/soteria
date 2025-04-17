#include "test2.h"

int main()
{
  int ret = compute();
  __assert__(ret == 12);
  return 0;
}
