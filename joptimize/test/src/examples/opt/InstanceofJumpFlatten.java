package joptimize.examples.opt;

class InstanceofJumpFlatten {
  static int simpleBar(int x){ return call(new InstanceImplA(), x); }
  static int simpleBaz(int x){ return call(new InstanceImplB(), x); }
  static int simpleQux(int x){ return call(new InstanceImplC(), x); }
  static int simpleBarMatch(int x){ return call(new InstanceImplA(), x); }
  static int simpleBazMatch(int x){ return call(new InstanceImplB(), x); }
  static int simpleQuxMatch(int x){ return call(new InstanceImplC(), x); }

  static int call(InstanceTrait b, int x){
    if (b instanceof InstanceImplA) return leaf1(x);
    else if (b instanceof InstanceImplB) return leaf2(x);
    else if (b instanceof InstanceImplC) return leaf3(x);
    else return 0;
  }

  static int leaf1(int x){ return x + 1; }
  static int leaf2(int x){ return x + 2; }
  static int leaf3(int x){ return x + 3; }
}


interface InstanceTrait{}
class InstanceImplA implements InstanceTrait{}
class InstanceImplB implements InstanceTrait{}
class InstanceImplC implements InstanceTrait{}