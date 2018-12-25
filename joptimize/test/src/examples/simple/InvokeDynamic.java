package joptimize.examples.simple;

public class InvokeDynamic {
    @joptimize.Test(inputs = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12})
    public static int lambda(int a) {
        int x = 1;
        java.util.function.IntSupplier r1 = () -> a + x;
        return r1.getAsInt();
    }
    @joptimize.Test(inputs = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12})
    public static int lambdaBoxed(int a) {
        int x = 1;
        java.util.function.Supplier<Integer> r1 = () -> a + x;
        return r1.get();
    }
    @joptimize.Test(inputs = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12})
    public static String concat(int lhs, int rhs){
        return lhs + " sep " + rhs;
    }
}