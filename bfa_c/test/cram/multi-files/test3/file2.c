#include "test3.h"
#include <stdio.h>

int add(int x, int y)
{
    return x + y;
}

int main()
{
    int ret = operate(add, 5, 3);
    __assert__(ret == 8);
    return 0;
}
