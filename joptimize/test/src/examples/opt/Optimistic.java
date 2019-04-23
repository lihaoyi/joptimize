package joptimize.examples.opt;

class Optimistic {

    @joptimize.Test(inputs = {1, 2, 3}, removedNumConst = {13}, addedNumConst = {14})
    static int trivial(int x) {
        int z = 13;
        return z + 1 + x ;
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

    @joptimize.Test(inputs = {1, 2, 3}, removedNumConst = {10}, addedNumConst = {8})
    static int branchConstant(int x) {
        int z = 9;
        if (z > 10){
            z = z + x;
        }
        return z - 1;
    }


    @joptimize.Test(
            inputs = {1, 2, 3, 4},
            checkRemoved = "joptimize.examples.opt.Optimistic.recursivePureConstant0"
    )
    public static int recursivePureConstant(int x, int y) {
        return recursivePureConstant0(x, y);
    }

    public static int recursivePureConstant0(int x, int y) {
        if (x > 2) return 1;
        else return recursivePureConstant0(x + 1, y);
    }
}
