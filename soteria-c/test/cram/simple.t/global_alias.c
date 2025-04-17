#include <stdlib.h>

extern void __assert__(int x);

int x;
int y;

int main()
{
  __assert__(&x != &y);

  return 0;
}