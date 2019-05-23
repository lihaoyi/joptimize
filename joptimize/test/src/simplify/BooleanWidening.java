package test.simplify;

class BooleanWidening {
    @test.Test(
            inputs = {1, 0},
            checkPresent = {"BooleanWidening.invert"}
    )
    static int simple(boolean b) {
        return invert(b) ? 1 : 2;
    }

    static boolean invert(boolean b) {
        return !b;
    }
}
