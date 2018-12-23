package joptimize.examples.narrow;

class Supertype2 {
  static int main(int x, int y){
    return call(new Bar(), x) + call(new Qux(), y);
    // (x + 1) + (y + 2)
  }

  static int call(Foo f, int n){ return call2(f, n); }
  static int call2(Foo f, int n){ return f.inc(n); }
}
