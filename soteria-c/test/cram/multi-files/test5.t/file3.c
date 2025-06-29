#include "test5.h"

int main()
{
  int ret = fnA() + fnB();
  __soteria___assert(ret == 28);
  return 0;
}