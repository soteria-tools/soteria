#include "test2.h"

static int helper()
{
  return 7;
}

int compute()
{
  return 5 + helper();
}
