int main()
{
  char *c = "abcd";
  char k[] = "abcd";
  __soteria___assert(c[2] == 'c');
  __soteria___assert(k[2] == 'c');
  *k = 'e'; // Should work!
  *c = 'e'; // Segmentation fault!
  return 0;
}