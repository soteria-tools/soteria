int x;

extern void __assert__(int x);

int main()
{
  int y;
  __assert__(&x != &y);
  return 0;
}