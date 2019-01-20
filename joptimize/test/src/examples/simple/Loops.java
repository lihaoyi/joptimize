package joptimize.examples.simple;


public class Loops {
    public static int nullFor(int a) {
        int c = 0;
        for (int i = 0; i > a; i++) c++;
        return c;
    }

    @joptimize.Test(inputs = {0, 1, 2, 3, 4, 5, 6, 7, 9})
    public static int basicFor(int a) {
        int c = 1;
        for (int i = 1; i < a; i++) c = c * i;
        return c;
    }


    @joptimize.Test(inputs = {0, 1, 2, 3, 4, 5, 6, 7, 9})
    public static int nestedFor(int a) {
        int c = 1;
        for (int i = 0; i < a; i++) {
            c += 1;
            for (int j = 0; j < i; j++) {
                c += j;
            }
        }
        return c;
    }


    @joptimize.Test(inputs = {0, 1, 2, 3, 4, 5, 6, 7, 9})
    public static int nestedFor2(int a) {
        int c = 1;
        for (int i = 0; i < a; i++) {
            c += 1;
            for (int j = 0; j < i; j++) {
                c += j;
            }
            for (int j = 0; j < i; j++) {
                c *= j;
            }
        }
        return c;
    }

    @joptimize.Test(inputs = {0, 1, 2, 3, 4, 5, 6, 7, 9})
    public static int liftableFor(int a) {
        int c0 = 0;
        int c = c0 + 3;

        for (int i = 0; i < a; i++) {
            int delta = a * 2;
            c = c + delta;
        }
        return c + 7;
    }

    @joptimize.Test(inputs = {0, 1, 2, 3, 4, 5, 6, 7, 9})
    public static int unliftableFor(int a) {
        int c0 = 0;
        int c = c0 + 3;

        for (int i = 0; i < a; i++) {
            int delta = i * 2;
            c = c + delta;
        }
        return c + 7;
    }

    @joptimize.Test(inputs = {0, 1, 2, 3, 4, 5, 6, 7, 9})
    public static int mixedLiftableFor(int a) {
        int c0 = 0;
        int c = c0 + 3;

        for (int i = 0; i < a; i++) {
            int liftable = a * 5;
            int unliftable = i * 7;
            int liftable2 = liftable + 9;
            int unliftable2 = unliftable + 11;
            c = c + liftable2 + unliftable2;
        }
        return c + 2;
    }

    @joptimize.Test(inputs = {0, 1, 2, 3, 4, 5, 6, 7, 9})
    public static int nestedMixedLiftableFor(int a) {
        int c0 = 0;
        int c = c0 + 3;

        for (int i = 0; i < a; i++) {
            int liftable = a * 5;
            int unliftable = i * 7;
            int liftable2 = liftable + 9;
            int unliftable2 = unliftable + 11;

            for(int j = 0; j < a; j++){
                int innerLiftable = a * 13;
                int innerHalfLiftable = i * 15;
                int innerLiftable2 = innerLiftable + 17;
                int innerHalfLiftable2 = liftable2 + unliftable2 + 19;
                int innerUnliftable = j + 21;

                c = c + innerLiftable2 + innerHalfLiftable2 + innerHalfLiftable + innerUnliftable;
            }
        }
        return c + 2;
    }


    @joptimize.Test(inputs = {0, 1, 2, 3, 4, 5, 6, 7, 9})
    public static int nestedUnliftableFor(int a) {
        int c = 1;

        for (int i = 0; i < a; i++) {
            for(int j = 0; j < a; j++){
                c = c * a;
            }
        }
        return c;
    }

    @joptimize.Test(inputs = {0, 1, 2, 3, 4, 5, 6, 7, 9})
    public static int nestedLiftableFor(int a) {
        int c = 1;
        for (int i = 0; i < a; i++) {
            for(int j = 0; j < a; j++){
                int e = a + 7;
                c += e;
            }
        }
        return c;
    }

    @joptimize.Test(inputs = {0, 1, 2, 3, 4, 5, 6, 7, 9})
    public static int nestedLiftableFor2(int a) {
        int c = 1;
        for (int i = 0; i < a; i++) {
            int e = a + 7;
            for(int j = 0; j < a; j++){
                c += e;
            }
        }
        return c;
    }


    public static int nullWhile(int a) {
        int c = 1;
        while (c > a) c++;
        return c;
    }

    @joptimize.Test(inputs = {0, 1, 2, 3, 4, 5, 6, 7, 9})
    public static int basicWhile(int a) {
        int c = 1;
        while (c < a) c = c * 2;
        return c;
    }

    @joptimize.Test(inputs = {0, 1, 2, 3, 4, 5, 6, 7, 9})
    public static int alternatingWhile(int a) {
        int c = 1;
        int b = 1;
        while (c < a) {
            b = c;
            c = c * 2;
        }
        return b;
    }

    @joptimize.Test(inputs = {3, 4, 5})
    public static double loopReturn1(int n0) {

        int errorSquared = 7;
        while (true) {
            if (errorSquared / n0 < 3) return errorSquared;
        }
    }

    @joptimize.Test(inputs = {1, 2, 4, 8, 50000})
    public static int loopReturn2(int n0) {
        int errorSquared = n0;
        while (true) {
            errorSquared = errorSquared - 1;
            if (errorSquared < 0) return errorSquared;
        }
    }

    @joptimize.Test(inputs = {-10, 0, 15, 300})
    public static double loopReturn3(int n0) {
        double guess = 2;

        while (true) {
            if (guess > n0) return guess;
            else guess = guess * guess;
        }
    }

    @joptimize.Test(inputs = {1, 9, 121, 10000})
    public static double sqrtFinder(int n0) {
        double n = (double)n0;
        double guess = n / 2 + 5;

        while (true) {
            double errorSquared = guess * guess - n;
            errorSquared = errorSquared * errorSquared;
            if (errorSquared / n < 0.1) return guess;
            else {
                guess = ((guess * guess) - n) / (2 * guess);
            }
        }
    }
}