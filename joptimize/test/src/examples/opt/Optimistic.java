package joptimize.examples.opt;

class Optimistic {

    @joptimize.Test(inputs = {1, 2, 3}, removedNumConst = {26}, addedNumConst = {20})
    static int trivial(int x) {
        int z = 13;
        return z + x;
    }

    @joptimize.Test(inputs = {1, 2, 3}, removedNumConst = {26}, addedNumConst = {20})
    static int loopConstant(int x) {
        int z = 13;
        while(x > 0){
            z = 26 - z;
            x -= 1;
        }
        return z + 7;
    }

    @joptimize.Test(inputs = {1, 2, 3}, removedNumConst = {26}, addedNumConst = {20})
    static int branchConstant(int x) {
        int z = 9;
        if (z > 10){
            z = z + x;
        }
        return z;
    }
//
//    @joptimize.Test(
//        inputs = {1, 2},
//        checkPresent = {"SimpleDce.call1", "SimpleDce.call2"},
//        checkRemoved = {"SimpleDce.call3"}
//    )
//    static int main(int x, int y) {
//        return call1(x) + call2(y);
//    }
//
//    static int call1(int x) {
//        return x + 1;
//    }
//
//    static int call2(int y) {
//        return y + 2;
//    }
//
//    static int call3(int z) {
//        return z + 2;
//    }
}
