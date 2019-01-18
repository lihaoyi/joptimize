package joptimize.examples.simple;

public class Hello {
    @joptimize.Test(inputs = {1, 2, 3, 4, 5, 6})
    public static int incrI(int n) {
        return n + 1;
    }
    @joptimize.Test(inputs = {1, 2, 3, 4, 5, 6})
    public static long incrJ(long n) {
        return n + 1;
    }
    @joptimize.Test(inputs = {1, 2, 3, 4, 5, 6})
    public static short incrS(short n) {
        return (short)(n + 1);
    }
    @joptimize.Test(inputs = {1, 2, 3, 4, 5, 6})
    public static float incrF(float n) {
        return n + 1;
    }
    @joptimize.Test(inputs = {1, 2, 3, 4, 5, 6})
    public static double incrD(double n) {
        return n + 1;
    }
    @joptimize.Test(inputs = {1, 2, 3, 4, 5, 6})
    public static int addI(int n, int m) {
        return n + m;
    }
    @joptimize.Test(inputs = {1, 2, 3, 4, 5, 6})
    public static long addJ(long n, long m) {
        return n + m;
    }
    @joptimize.Test(inputs = {1, 2, 3, 4, 5, 6})
    public static short addS(short n, short m) {
        return (short)(n + m);
    }
    @joptimize.Test(inputs = {1, 2, 3, 4, 5, 6})
    public static float addF(float n, float m) {
        return n + m;
    }
    @joptimize.Test(inputs = {1, 2, 3, 4, 5, 6})
    public static double addD(double n, double m) {
        return n + m;
    }

}
