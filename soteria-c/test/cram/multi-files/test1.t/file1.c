#include "test1.h"

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
    __soteria___assert(test() == 42);
    update();
    __soteria___assert(test() == 1024);
    return 0;
}
