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
    @joptimize.Test(inputs = {1, 2, 3, 4, 5, 6})
    public static int localI(int n, int m) {
        int x = n + m;
        return x * x;
    }
    @joptimize.Test(inputs = {1, 2, 3, 4, 5, 6})
    public static long localJ(long n, long m) {
        long x = n + m;
        return x * x;
    }
    @joptimize.Test(inputs = {1, 2, 3, 4, 5, 6})
    public static short localS(short n, short m) {
        short x = (short)(n + m);
        return (short)(x * x);
    }
    @joptimize.Test(inputs = {1, 2, 3, 4, 5, 6})
    public static float localF(float n, float m) {
        float x = n + m;
        return x * x;
    }
    @joptimize.Test(inputs = {1, 2, 3, 4, 5, 6})
    public static double localD(double n, double m) {
        double x = n + m;
        return x * x;
    }

}
