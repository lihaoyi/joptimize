package test.simplify;

class BooleanJumpFlatten {
    @test.Test(
        inputs = {1, 2},
        checkPresent = {"BooleanJumpFlatten.leaf1"},
        checkRemoved = {"BooleanJumpFlatten.leaf2"}
    )
    static int simpleTrue(int x, int y) {
        return call(true, x, y);
    }

    @test.Test(
            inputs = {1, 2},
            checkPresent = {"BooleanJumpFlatten.leaf2"},
            checkRemoved = {"BooleanJumpFlatten.leaf1"}
    )
    static int simpleFalse(int x, int y) {
        return call(false, x, y);
    }

    static int call(boolean b, int x, int y) {
        if (b) {
            return leaf1(x);
        } else {
            return leaf2(y);
        }
    }
    @test.Test(inputs = {1, 2})
    static int nested1(int x, int y) {
        return nestedCall(false, false, x, y);
    }
    @test.Test(inputs = {1, 2})
    static int nested2(int x, int y) {
        return nestedCall(false, true, x, y);
    }

    @test.Test(inputs = {1, 2})
    static int nested3(int x, int y) {
        return nestedCall(true, false, x, y);
    }
    @test.Test(inputs = {1, 2})
    static int nested4(int x, int y) {
        return nestedCall(true, true, x, y);
    }

    static int nestedCall(boolean b, boolean c, int x, int y) {
        int res = 0;

        if (b) {
            res = x;
        } else {
            if (c) res = y;
            else res = leaf2(y);
        }
        return res;
    }

    static int leaf1(int x) {
        return x + 1;
    }

    static int leaf2(int y) {
        return y + 2;
    }
}
