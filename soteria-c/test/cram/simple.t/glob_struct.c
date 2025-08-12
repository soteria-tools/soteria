void __soteria___assert(int cond);

struct X
{
  int a;
  long b;
};

struct X x;

int main()
{
  struct X y = {.a = 1, .b = 2};
  struct X z = {.b = 3, .a = 4};
  __soteria___assert(x.a == 0);
  __soteria___assert(z.a == 4);
  return 0;
}