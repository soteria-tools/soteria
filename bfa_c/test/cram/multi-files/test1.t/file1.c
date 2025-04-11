#include "test1.h"

extern void __assert__(int x);

int test()
{
    return exposed();
}

void update()
{
    glob = 1024;
}

int main()
{
    __assert__(test() == 42);
    update();
    __assert__(test() == 1024);
    return 0;
}
