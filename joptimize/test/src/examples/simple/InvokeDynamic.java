package joptimize.examples.simple;

public class InvokeDynamic {
    interface IntSupplier{
        int getAsInt();
    }

    @joptimize.Test(inputs = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12})
    public static int lambda(int a) {
        int x = 1;
        IntSupplier r1 = () -> a + x;
        return r1.getAsInt();
    }

    interface DoubleToIntFunction{
        int applyAsInt(double value);
    }
    @joptimize.Test(inputs = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12})
    public static int lambdaArg(int a) {
        int x = 1;
        DoubleToIntFunction r1 = d -> (int)d + x;
        return r1.applyAsInt(10.0 / a);
    }


    interface Supplier<T> {
        T get();
    }
    @joptimize.Test(inputs = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12})
    public static int lambdaBoxed(int a) {
        int x = 1;
        Supplier<Integer> r1 = () -> a + x;
        return r1.get();
    }


    interface BiFunction<T, U, R> {
        R apply(T t, U u);
    }

    @joptimize.Test(inputs = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12})
    public static int lambdaBoxedArgs(int a) {
        int x = 1;
        BiFunction<Double, Float, Integer> r1 = (d, f) -> (int)(d + f) + x;
        return r1.apply(10.0 / a, 5.0f / a);
    }

    @joptimize.Test(inputs = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12})
    public static String concat(int lhs, int rhs){
        return lhs + " sep " + rhs;
    }
}