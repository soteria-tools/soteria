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
  __soteria___assert(sizeof(int) == 4);
  __soteria___assert(_Alignof(int) == 4);
  __soteria___assert(sizeof(struct S) == 8);
  __soteria___assert(sizeof(union U) == sizeof(int));
  __soteria___assert(_Alignof(union U) == _Alignof(int));
  return 0;
}