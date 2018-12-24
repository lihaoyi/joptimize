package joptimize.examples.simple;

public class InvokeDynamic {
    public static int lambda(int a) {
        int x = 1;
        java.util.function.IntSupplier r1 = () -> a + x;
        return r1.getAsInt();
    }
    public static int lambdaBoxed(int a) {
        int x = 1;
        java.util.function.Supplier<Integer> r1 = () -> a + x;
        return r1.get();
    }
    public static String concat(String lhs, String rhs){
        return lhs + " sep " + Integer.parseInt(rhs);
    }
}