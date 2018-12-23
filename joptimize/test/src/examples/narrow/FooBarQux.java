package joptimize.examples.narrow;

interface Foo{
  int inc(int n);
}
class Bar implements Foo{
  public int inc(int n){ return n + 1; }
}
class Qux implements Foo{
  public int inc(int n){ return n + 2; }
}