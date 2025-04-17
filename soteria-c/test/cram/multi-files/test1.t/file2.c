// #include "test1.h"

int glob = 42;
int *ref = &glob;

int exposed()
{
  return *ref;
}
