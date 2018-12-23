package joptimize.examples.opt;

class BooleanJumpFlatten {
    static int simpleTrue(int x, int y){
        return call(true, x, y);
    }
    static int simpleFalse(int x, int y){
        return call(false, x, y);
    }
    static int call(boolean b, int x, int y){
        if (b) {
            return leaf1(x);
        }
        else {
            return leaf2(y);
        }
    }
    static int leaf1(int x){ return x + 1; }
    static int leaf2(int y){ return y + 2;}
}
