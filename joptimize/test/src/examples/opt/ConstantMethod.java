package joptimize.examples.opt;

class ConstantMethod {
    static int intMain(boolean b) {
        return b ? callInt(true) : callInt(false);
    }

    static int callInt(boolean b) {
        return b ? 1 : 2;
    }

    static boolean boolMain(boolean b) {
        return b ? callBool(true) : callBool(false);
    }

    static boolean callBool(boolean b) {
        return b ? !b : !b;
    }

    static boolean impureMain(boolean b) {
        return b ? callImpure(true) : callImpure(false);
    }

    static boolean callImpure(boolean b) {
        System.currentTimeMillis();
        return b ? !b : !b;
    }
}