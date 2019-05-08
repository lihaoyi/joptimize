package test.optimistic;

class Recursive {

    @test.Test(
            inputs = {1, 2, 3, 4},
            checkRemoved = "joptimize.test.simplify.Recursive.recursivePureConstant0"
    )
    public static int recursivePureConstant(int x, int y) {
        return recursivePureConstant0(x, y);
    }

    public static int recursivePureConstant0(int x, int y) {
        if (x > 2) return 1;
        else return recursivePureConstant0(x + 1, y);
    }


    @test.Test(
            inputs = {1, 2, 3, 4},
            checkRemoved = {
                    "joptimize.test.simplify.Recursive.mutualRecursivePureConstant1",
                    "joptimize.test.simplify.Recursive.mutualRecursivePureConstant2"
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


    @test.Test(
            inputs = {1, 2, 3, 4},
            checkPresent = {
                    "joptimize.test.simplify.Recursive.generalRecursiveHelper"
            }
    )
    public static int generalRecursive(int x, int y) {
        return generalRecursive0(x, y);
    }

    public static int generalRecursive0(int x, int y) {
        if (x == 0) return y;
        else {
            int z = generalRecursive0(x - 1, y * 2);
            return generalRecursiveHelper(z);
        }
    }
    public static int generalRecursiveHelper(int z) {
        return z * z;
    }


    @test.Test(
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


    @test.Test(
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
