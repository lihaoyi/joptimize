package joptimize.examples.simple;


public class IfElse {
    @joptimize.Test(inputs = {10, 11, 10, 9})
    public static int basicIf(int a, int b) {
        if (a < b) return a;
        else return -a;
    }

    @joptimize.Test(inputs = {0, 1, 2, 3, 4, 5})
    public static int ifAssign(int a) {
        int b = 1;
        if (a >= 1) b += 1;
        else b += 2;

        return b;
    }

    @joptimize.Test(inputs = {10, 11, -9, 9})
    public static int ifNonIntZero(int a) {
        if (((byte) a) > 0) return a;
        else return -a;
    }

    @joptimize.Test(inputs = {10, 11, 10, 9, 200, 200})
    public static int ifNonIntBinary(int a, int b) {
        if (((byte) a) > (short) b) return a;
        else return -a;
    }

    @joptimize.Test(inputs = {3, 2, 2, 2, 1, 2})
    public static int ifElseIf(int a, int b) {
        if (a > b) return a;
        else if (a == b) return -a;
        else return b;
    }

    @joptimize.Test(inputs = {2, 1, 13, 13, 9, 9, 11, 11, 1, 10})
    public static int ifElseIfBig(int a, int b) {
        if (a > b) return 1;
        else if (a > 12) return 2;
        else if (b < 10) return 3;
        else if (b == a) return 4;
        else if (b > a) return 5;
        else if (a == 10) return 6;
        else if (b == 312) return 7;
        else return 8;
    }

    public static int mathMin(int a, int b) {
        return Math.min(a, b);
    }
}