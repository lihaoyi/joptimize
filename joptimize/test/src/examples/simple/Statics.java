package joptimize.examples.simple;

public class Statics {
    @joptimize.Test(inputs = {1, 2, 3, 4, 5})
    public static int helloWorld(int n) {
        return timesTwo(n);
    }

    @joptimize.Test(inputs = {1, 2, 3, 4, 5})
    public static int timesTwo(int n) {
        return n * 2;
    }

    @joptimize.Test(inputs = {1, 1, 1, 2, 2, 1, 2, 2})
    public static int add(int a, int b) {
        return a + b;
    }

    @joptimize.Test(inputs = {1, 1, 5, 2})
    public static int helloWorld2(int a, int b) {
        return timesTwo2(a, b);
    }

    @joptimize.Test(inputs = {1, 1, 5, 2})
    public static int timesTwo2(int a, int b) {
        return (a - b) * 2;
    }

    @joptimize.Test(inputs = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10})
    public static int tailFactorial(int n) {
        if (n == 1) {
            return 1;
        } else {
            return n * tailFactorial(n - 1);
        }
    }

    @joptimize.Test(inputs = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10})
    public static int fibonacci(int n) {
        if (n == 1 || n == 0) {
            return 1;
        } else {
            return fibonacci(n - 1) + fibonacci(n - 2);
        }
    }

    @joptimize.Test(inputs = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10})
    public static int call(int x) {
        return x + 1;
    }

    @joptimize.Test(inputs = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10})
    public static int callAtPhiBoundary(int i) {

        int size = (i < 0) ? 1 : call(i);
        return size;
    }
}
