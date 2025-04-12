#include "test5.h"

int main()
{
  int ret = fnA() + fnB();
  __assert__(ret == 28);
  return 0;
}