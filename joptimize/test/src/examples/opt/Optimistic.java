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


    @joptimize.Test(
            inputs = {1, 2, 3, 4},
            checkRemoved = {
                    "joptimize.examples.opt.Optimistic.mutualRecursivePureConstant1",
                    "joptimize.examples.opt.Optimistic.mutualRecursivePureConstant2"
            }
    )
    public static int mutualRecursivePureConstant(int x, int y) {
        return mutualRecursivePureConstant1(x, y);
    }

    public static int mutualRecursivePureConstant1(int x, int y) {
        if (x > 2) return 1;
        else return mutualRecursivePureConstant2(x + 1, y);
    }

    public static int mutualRecursivePureConstant2(int x, int y) {
        if (x > 2) return 1;
        else return mutualRecursivePureConstant1(x + 1, y);
    }


    @joptimize.Test(
            inputs = {1, 2, 3, 4}
    )
    public static int generalRecursive(int x, int y) {
        return generalRecursive0(x, y);
    }

    public static int generalRecursive0(int x, int y) {
        if (x == 0) return y;
        else {
            int z = generalRecursive0(x - 1, y) + generalRecursive0(x - 1, y);

            return z;
        }
    }


    @joptimize.Test(
            inputs = {1, 2, 3, 4},
            removedNumConst = {123},
            addedNumConst = {456}
    )
    public static int recursivePureDeadArg(int x) {
        return recursivePureDeadArg0(x, 123, 456);
    }

    public static int recursivePureDeadArg0(int x, int y, int z) {
        if (x > 2) return x + z;
        else return recursivePureDeadArg0(x + 1, y, z);
    }


    @joptimize.Test(
            inputs = {1, 2, 3, 4},
            removedNumConst = {123},
            addedNumConst = {456}
    )
    public static int mutualRecursivePureDeadArg(int x) {
        return mutualRecursivePureDeadArg1(x, 123, 456);
    }

    public static int mutualRecursivePureDeadArg1(int x, int y, int z) {
        if (x > 2) return x + z;
        else return mutualRecursivePureDeadArg2(x + 1, y, z);
    }

    public static int mutualRecursivePureDeadArg2(int x, int y, int z) {
        if (x > 2) return x + z;
        else return mutualRecursivePureDeadArg1(x + 1, y, z);
    }
}
