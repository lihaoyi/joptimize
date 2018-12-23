package joptimize.examples.narrow;

class ForceWide{
    static Foo f1 = new Bar();
    static Foo f2 = new Qux();
    static int main(int x, int y){
    return call(f1, x) + call(f2, y);
    // (x + 1) + (y + 2)
  }

  static int call(Foo f, int n){ return f.inc(n); }
}

