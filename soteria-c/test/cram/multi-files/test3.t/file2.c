#include "test3.h"

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
    __soteria___assert(ret == 8);
    return 0;
}
