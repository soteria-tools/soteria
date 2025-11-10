#include <string.h>

int main()
{
  char a[] = {'a', 'b', 'c', '\0'};
  char b[] = {'a', 'b', 'd', '\0'};
  __soteria___assert(strcmp(a, b) < 0);
  __soteria___assert(strcmp(b, a) > 0);
  __soteria___assert(strcmp(a, a) == 0);
  return 0;
}