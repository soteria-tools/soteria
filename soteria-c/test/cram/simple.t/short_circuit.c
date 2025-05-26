#include <stdlib.h>

int main()
{
  int y = 2;
  int *z = (int *)NULL;
  int _ = y > 0 || (*z);
  _ = y < 1 && (*z);
  _ = y > 0 && y < 1;
  return 0;
}