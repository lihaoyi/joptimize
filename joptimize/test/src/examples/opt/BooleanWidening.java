package joptimize.examples.opt;

class BooleanWidening {
    static int simple(boolean b) {
        return invert(b) ? 1 : 2;
    }

    static boolean invert(boolean b) {
        return !b;
    }
}
