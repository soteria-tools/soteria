#include <stdlib.h>
#include <string.h>

int main()
{
  int *p = malloc(10 * sizeof(int));
  if (!p)
    return 0;
  memset(p, 0, 10 * sizeof(int));
  for (int i = 0; i < 10; i++)
  {
    __soteria___assert(p[i] == 0);
  }
  return 0;
}