package joptimize.examples.narrow;

class NarrowReturn {
    static int main(int x, int y){
    return call(wrap(new Bar()), x) + call(wrap(new Qux()), y);
  }

    static int call(Foo f, int n){ return f.inc(n); }

  static Foo wrap(Foo f){ return f; }
}
