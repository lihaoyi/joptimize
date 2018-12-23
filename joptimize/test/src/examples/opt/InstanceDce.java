package joptimize.examples.opt;

class InstanceDce {
  static FooTwo unknown1 = new BarTwo();
  static FooTwo unknown2 = new QuxTwo();
  static int simple1(int x, int y){ return new BarTwo().incA(x) + new QuxTwo().incB(y); }
  static int simple2(int x, int y){ return new BarTwo().incA(x) + new BarTwo().incB(y); }
  static int simple3(int x, int y){ return new QuxTwo().incA(x) + new QuxTwo().incB(y); }

  static int single1(int x, int y){ return new BarTwo().incA(x) + new BarTwo().incA(y); }
  static int single2(int x, int y){ return new BarTwo().incB(x) + new BarTwo().incB(y); }
  static int single3(int x, int y){ return new QuxTwo().incA(x) + new QuxTwo().incA(y); }
  static int single4(int x, int y){ return new QuxTwo().incB(x) + new QuxTwo().incB(y); }

  static int unknown1(int x, int y){ return unknown1.incA(x) + unknown2.incA(y); }
  static int unknown2(int x, int y){ return unknown1.incB(x) + unknown2.incB(y); }
  static int unknown3(int x, int y){ return unknown1.incA(x) + unknown2.incB(y); }

}
interface FooTwo{
  int incA(int n);
  int incB(int n);
}
class BarTwo implements FooTwo{
  public int incA(int n){ return n + 1;}
  public int incB(int n){ return n + 2;}
}
class QuxTwo implements FooTwo{
  public int incA(int n) { return n + 3; }
  public int incB(int n) { return n + 4; }
}