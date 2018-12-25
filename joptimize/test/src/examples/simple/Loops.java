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