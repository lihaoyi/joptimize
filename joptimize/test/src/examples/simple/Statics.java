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

    @joptimize.Test(inputs = {1, 1, 5, 2})
    public static int helloWorld2(int a, int b) {
        return timesTwo2(a, b);
    }

    @joptimize.Test(inputs = {1, 1, 5, 2})
    public static int timesTwo2(int a, int b) {
        return (a - b) * 2;
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

    @joptimize.Test(inputs = {1, 2, 3, 4, 5})
    public static int[] voidCall(int n) {
        int[] x = new int[n];
        if (n % 2 == 0) voidMethod(x, n);
        return x;
    }

    public static void voidMethod(int[] x, int n) {
        x[0] += n;
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
    public static long tailFactorialLong(long n) {
        if (n == 1) {
            return 1;
        } else {
            return n * tailFactorialLong(n - 1);
        }
    }

    @joptimize.Test(inputs = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10})
    public static int tailFactorialVoid(int n) {
        int[] x = new int[]{0};
        tailFactorialVoid0(n, x);
        return x[0];
    }

    public static void tailFactorialVoid0(int n, int[] x) {
        if (n == 1) {
            x[0] = 1;
        } else {
            tailFactorialVoid0(n - 1, x);
            x[0] *= n;
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

    @joptimize.Test(inputs = {1})
    public static int staticInit(int n) {
        int x = staticInitValue;
        int y = StaticInit.get();
        int z = staticInitValue;
        return n + x + y + z;

    }
    static int staticInitValue = 10;
    static class StaticInit{
        static int dummy = 123;

        static int get(){
            return 100;
        }

        static{
            System.out.println("Hello!");
            Statics.staticInitValue = 1000;
        }
    }
}
