#include "test3.h"
#include <stdio.h>

typedef int (*operation)(int, int);
int operate(operation op, int a, int b);

// ...existing code...
int operate(operation op, int a, int b)
{
    return op(a, b);
}

int main()
{
    int ret = operate(add, 5, 3);
    __assert__(ret == 8);
    return 0;
}
