package joptimize.examples.simple;

public class InvokeDynamic {
    public static int lambda(int a) {
        int x = 1;
        java.util.function.IntSupplier r1 = () -> a;
        return r1.getAsInt() + x;
    }
    public static String concat(String lhs, String rhs){
        return lhs + rhs;
    }
}