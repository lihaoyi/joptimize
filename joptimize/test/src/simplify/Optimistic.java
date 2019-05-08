package test.simplify;

class Optimistic {

    @test.Test(inputs = {1, 2, 3}, removedNumConst = {13}, addedNumConst = {14})
    static int trivial(int x) {
        int z = 13;
        return z + 1 + x ;
    }

    @test.Test(inputs = {1, 2, 3}, removedNumConst = {26}, addedNumConst = {20})
    static int loopConstant(int x) {
        int z = 13;
        while(x > 0){
            z = 26 - z;
            x -= 1;
        }
        return z + 7;
    }

    @test.Test(inputs = {1, 2, 3}, removedNumConst = {10}, addedNumConst = {8})
    static int branchConstant(int x) {
        int z = 9;
        if (z > 10){
            z = z + x;
        }
        return z - 1;
    }


    @test.Test(
            inputs = {1, 2, 3, 4},
            checkRemoved = "Optimistic.recursivePureConstant0"
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
                    "Optimistic.mutualRecursivePureConstant1",
                    "Optimistic.mutualRecursivePureConstant2"
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
                    "Optimistic.generalRecursiveHelper"
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



    @test.Test(
            // These numbers should *not* get constant folded, as there are two
            // subtypes of `Foo` and we do not know which one `.call` is called on
            addedNumConst = {123, 456}
    )
    public static int implementLate1() {
        Foo foo = new Bar();
        int first = foo.call() + 123;
        foo = new Qux();
        int second = foo.call() + 456;
        return first + second;
    }

    @test.Test(
            // These numbers should *not* get constant folded, as there are two
            // subtypes of `Foo` and we do not know which one `.call` is called on
            addedNumConst = {123}
    )
    public static int implementLate2() {
        Foo foo = new Bar();
        int first = foo.call() + 123;
        foo = new Qux();
        return first;
    }


    @test.Test(
            // These numbers should *not* get constant folded, as there are two
            // subtypes of `Foo` and we do not know which one `.call` is called on
            addedNumConst = {123}
    )
    public static int implementLate3() {
        Foo foo = new Bar();
        foo = new Qux();
        int first = foo.call() + 123;
        return first;
    }

    @test.Test(
            // Make sure that in this case, since `Bar` is the only subtype
            // instantiated, we can constant fold `1 + 123` into `124`
            removedNumConst = {123},
            addedNumConst = {124}
    )
    public static int implementLate4() {
        Foo foo = new Bar();
        int first = foo.call() + 123;
        return first;
    }
    static abstract class Foo{
        abstract int call();
    }
    static class Bar extends Foo{
        int call(){
            return 1;
        }
    }
    static class Qux extends Foo{
        int call(){
            return 2;
        }
    }
}
