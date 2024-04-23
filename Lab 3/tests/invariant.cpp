int foo(int a) {
  int x = 4, z = 1, w = 0;
  for (int y = 0; y < a; y++) {
    x = a;
    z = 10;
    w += y;
  }
  return x + z + w;
}

int bar() {
  int x = 4, z = 1, w = 0, a = 10000;
  for (int y = 0; y < a; y++) {
    x = a;
    z = 10;
    w += y;
  }
  return x + z + w;
}

int main() { return foo(100); }
