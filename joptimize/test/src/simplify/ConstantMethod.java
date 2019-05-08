package test.simplify;

class ConstantMethod {
    @test.Test(
        inputs = {0, 1},
        checkPresent = {"ConstantMethod.intMain0"},
        checkRemoved = {"ConstantMethod.callInt"},
        checkNotMangled = {"ConstantMethod.callInt"}
    )
    static int intMain0(int b) {
        return callInt(true);
    }

    @test.Test(
        inputs = {0, 1},
        checkPresent = {"ConstantMethod.intMain"},
        checkRemoved = {"ConstantMethod.callInt"},
        checkNotMangled = {"ConstantMethod.callInt"}
    )
    static int intMain(int b) {
        return b != 0 ? callInt(true) : callInt(false);
    }

    @test.Test(
        inputs = {0, 1},
        checkPresent = {"ConstantMethod.nonFoldedIntMain", "ConstantMethod.callInt"},
        checkNotMangled = {"ConstantMethod.callInt"}
    )

    static int nonFoldedIntMain(int b) {
        boolean bool = b != 0 ? true : false;
        return callInt(bool);
    }

    static int callInt(boolean b) {
        return b ? 1 : 2;
    }

    @test.Test(
        inputs = {0, 1},
        checkPresent = {"ConstantMethod.boolMain"},
        checkRemoved = {"ConstantMethod.callBool"},
        checkNotMangled = {"ConstantMethod.callBool"}
    )
    static boolean boolMain(int b) {
        return b != 0 ? callBool(true) : callBool(false);
    }

    static boolean callBool(boolean b) {
        return b ? !b : !b;
    }

    @test.Test(
        inputs = {0, 1},
        checkPresent = {"ConstantMethod.impureMain"},
        checkMangled = {"ConstantMethod.callImpure"}
    )
    static boolean impureMain(int b) {
        return b != 0 ? callImpure(true) : callImpure(false);
    }

    static boolean callImpure(boolean b) {
        System.currentTimeMillis();
        return !b;
    }
}