void memcpy(void*, void*, __cerbty_size_t);
void* malloc(__cerbty_size_t);

int main() {
  
  int* x = malloc(3 * sizeof(int));
  if (!x) return 2;
  x[0] = 0;
  x[1] = 1;
  int* y = malloc(3 * sizeof(int));
  if (!y) return 3;
  memcpy(y, x, 2 * sizeof(int));
  return ((x[0] == y[0]) && (x[1] && y[1]));
  
}