void *malloc(__cerbty_size_t size);
void free(void *ptr);

struct test
{
  char x;
  char y;
};

int main()
{
  struct test t;
  t.x = 1;
  t.y = 2;
  __soteria___assert((&t)->x + t.y == 3);

  struct test *pt = malloc(sizeof(struct test));
  if (!pt)
    return 1;
  pt->x = 12;
  pt->y = 25;
  __soteria___assert(pt->y - pt->x == 13);
  __soteria___assert((&pt->y) - (&pt->x) == 1);
  free(pt);
  return 0;
}