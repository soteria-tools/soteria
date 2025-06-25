int __assert__(int);

struct S
{
  int x;
  char y;
};

union U
{
  char c;
  int i;
};

int main()
{
  __assert__(sizeof(int) == 4);
  __assert__(_Alignof(int) == 4);
  __assert__(sizeof(struct S) == 8);
  __assert__(sizeof(union U) == sizeof(int));
  __assert__(_Alignof(union U) == _Alignof(int));
  return 0;
}