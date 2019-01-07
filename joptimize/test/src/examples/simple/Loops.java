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
        for (int i = 0; i < a; i++) c = c * 2;
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
    public static int nestedLiftableFor(int a) {
        int c0 = 0;
        int c = c0 + 3;

        for (int i = 0; i < a; i++) {
            int liftable = a * 5;
            int unliftable = i * 7;
            int liftable2 = liftable + 9;
            int unliftable2 = unliftable + 11;

            for(int j = 0; j < a; j++){
                int innerLiftable = a * 13;
                int innerUnliftable = i * 15;
                int innerLiftable2 = innerLiftable + 17;
                int innerUnliftable2 = innerUnliftable + 19;
                int innerHalfLiftable = liftable2 + unliftable2;
                c = c + innerLiftable2 + innerUnliftable2 + innerHalfLiftable;
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

    @joptimize.Test(inputs = {121})
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