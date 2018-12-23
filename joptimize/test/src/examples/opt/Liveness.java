package joptimize.examples.opt;

class Liveness {
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


    static int simple2a(int i) {
        return terminal(true, pureButNotConstant(i), pureButNotConstant2(i));
    }

    static int simple2b(int i) {
        return terminal(false, pureButNotConstant(i), pureButNotConstant2(i));
    }

    static int terminal(boolean b, int i, int j) {
        return b ? i : j;
    }

    static int chained(int i) {
        pureButNotConstant(((i * i) * 2) - i);
        return i + 1;
    }

    static int chained2a(int i) {
        return terminal(true, pureButNotConstant(i), pureButNotConstant2(i * 2));
    }

    static int chained2b(int i) {
        return terminal(false, pureButNotConstant((i - 1) * (i - 5)), pureButNotConstant2(i));
    }

}