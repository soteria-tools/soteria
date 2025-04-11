#include "test2.h"

static int helper()
{
  return 5;
}

int compute()
{
  return 5 + helper();
}
