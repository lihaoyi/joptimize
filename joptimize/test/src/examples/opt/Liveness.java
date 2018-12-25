package joptimize.examples.opt;

class Liveness {
    @joptimize.Test(
        inputs = {1, 2},
        checkPresent = {"Liveness.simple"},
        checkRemoved = {"Liveness.pureButNotConstant"}
    )
    static int simple(int i) {
        pureButNotConstant(i);
        return i + 1;
    }

    static int pureButNotConstant(int i) {
        return i - 1;
    }

    static int pureButNotConstant2(int i) {
        return i + 1;
    }

    @joptimize.Test(
        inputs = {1, 2},
        checkPresent = {"Liveness.simple2a", "Liveness.pureButNotConstant"},
        checkRemoved = {"Liveness.pureButNotConstant2"}
    )
    static int simple2a(int i) {
        return terminal(true, pureButNotConstant(i), pureButNotConstant2(i));
    }


    @joptimize.Test(
        inputs = {1, 2},
        checkPresent = {"Liveness.simple2b", "Liveness.pureButNotConstant2"},
        checkRemoved = {"Liveness.pureButNotConstant"}
    )
    static int simple2b(int i) {
        return terminal(false, pureButNotConstant(i), pureButNotConstant2(i));
    }

    static int terminal(boolean b, int i, int j) {
        return b ? i : j;
    }

    @joptimize.Test(
            inputs = {1, 2},
            checkPresent = {"Liveness.chained"},
            checkRemoved = {"Liveness.pureButNotConstant"}
    )
    static int chained(int i) {
        pureButNotConstant(((i * i) * 2) - i);
        return i + 1;
    }

    @joptimize.Test(
            inputs = {1, 2},
            checkPresent = {"Liveness.chained2a", "Liveness.pureButNotConstant"},
            checkRemoved = {"Liveness.pureButNotConstant2"}
    )
    static int chained2a(int i) {
        return terminal(true, pureButNotConstant(i), pureButNotConstant2(i * 2));
    }

    @joptimize.Test(
            inputs = {1, 2},
            checkPresent = {"Liveness.chained2b", "Liveness.pureButNotConstant2"},
            checkRemoved = {"Liveness.pureButNotConstant"}
    )
    static int chained2b(int i) {
        return terminal(false, pureButNotConstant((i - 1) * (i - 5)), pureButNotConstant2(i));
    }

}