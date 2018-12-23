package joptimize.examples.narrow;

class Parametric {
  static int main(int x, int y){
    return call(new Bar(), x) + call(new Qux(), y);
    // (x + 1) + (y + 2)
  }

  static <T extends Foo> int call(T f, int n){ return f.inc(n); }
}
