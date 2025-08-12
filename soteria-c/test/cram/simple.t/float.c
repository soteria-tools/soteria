void __soteria___assert(int);
void *calloc(__cerbty_size_t, __cerbty_size_t);

float f;

int main()
{
  float x = 1.0f;
  float y = 2.0f;
  float *f = (float *)calloc(1, sizeof(float));
  if (!f)
  {
    return 1;
  }

  __soteria___assert(y > x);
  __soteria___assert(*f == 0.0f);
}