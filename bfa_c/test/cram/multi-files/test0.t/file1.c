#include "test0.h"

static int x = 12;

int get_x()
{
  return x;
}

int get_xx()
{
  return get_x();
}