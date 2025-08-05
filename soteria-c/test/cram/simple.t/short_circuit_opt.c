// Test that we do optimise short circuit evaluation if the LHS is not side-effectful.

int main()
{
  int x = __soteria___nondet_int();
  int y = __soteria___nondet_int();
  // If this returns a single branch, optimisation is working.
  return x && y;
}