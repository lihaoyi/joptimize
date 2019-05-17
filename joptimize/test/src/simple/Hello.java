package test.simple;

public class Hello {
    @test.Test(inputs = {0, 1, 2, 3})
    public static int incrI(int n) {
        return n + 1;
    }
    @test.Test(inputs = {0, 1, 2, 3})
    public static long incrJ(long n) {
        return n + 1;
    }
    @test.Test(inputs = {0, 1, 2, 3})
    public static short incrS(short n) {
        return (short)(n + 1);
    }
    @test.Test(inputs = {0, 1, 2, 3})
    public static float incrF(float n) {
        return n + 1;
    }
    @test.Test(inputs = {0, 1, 2, 3})
    public static double incrD(double n) {
        return n + 1;
    }
    @test.Test(inputs = {0, 1, 2, 3})
    public static int addI(int n, int m) {
        return n + m;
    }
    @test.Test(inputs = {0, 1, 2, 3})
    public static long addJ(long n, long m) {
        return n + m;
    }
    @test.Test(inputs = {0, 1, 2, 3})
    public static short addS(short n, short m) {
        return (short)(n + m);
    }
    @test.Test(inputs = {0, 1, 2, 3})
    public static float addF(float n, float m) {
        return n + m;
    }
    @test.Test(inputs = {0, 1, 2, 3})
    public static double addD(double n, double m) {
        return n + m;
    }
    @test.Test(inputs = {0, 1, 2, 3})
    public static int localI(int n, int m) {
        int x = n + m;
        return x * x;
    }
    @test.Test(inputs = {0, 1, 2, 3})
    public static long localJ(long n, long m) {
        long x = n + m;
        return x * x;
    }
    @test.Test(inputs = {0, 1, 2, 3})
    public static short localS(short n, short m) {
        short x = (short)(n + m);
        return (short)(x * x);
    }
    @test.Test(inputs = {0, 1, 2, 3})
    public static float localF(float n, float m) {
        float x = n + m;
        return x * x;
    }
    @test.Test(inputs = {0, 1, 2, 3})
    public static double localD(double n, double m) {
        double x = n + m;
        return x * x;
    }

    @test.Test(inputs = {0, 1, 2, 3})
    public static int overrideArg1(int i) {
        i = 0;

        return i;
    }

    @test.Test(inputs = {0, 1, 2, 3})
    public static int overrideArg2(int i) {
        i = i + 10;

        return i;
    }

    @test.Test(inputs = {0, 1, 2, 3})
    public static boolean overrideArg3(int i) {
        i = 0;

        return i > 10;
    }

    @test.Test(inputs = {0, 1, 2, 3})
    public static int overrideArg4(int i) {
        i = 0;

        return i + 10;
    }

    @test.Test(inputs = {0, 1, 2, 3})
    public static boolean overrideArg5(int i) {
        i = i + 10;

        return i > 10;
    }


    @test.Test(inputs = {0, 1, 2, 3})
    public static int overrideArg6(int i) {
        i = i + 10;

        return i + 10;
    }

}
