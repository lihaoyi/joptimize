package joptimize.examples.simple;


import java.io.IOException;

public class Exceptions {
    @joptimize.Test(inputs = {1})
    public static int throwCatch0(int a) {
        try {
            throw new Exception();
        } catch (Exception e) {
            return 1;
        }
    }

    @joptimize.Test(inputs = {1})
    public static int throwCatch1(int a) {
        try {
            if (a > 0) throw new Exception();
        } catch (Exception e) {
            return 1;
        }
        return 2;
    }

    @joptimize.Test(inputs = {1})
    public static int throwCatch2(int a) {
        int b = a >= 1 ? 2 : 3;

        try {
            if (a > 0) throw new Exception();
        } catch (Exception e) {
        }
        return b;
    }

    @joptimize.Test(inputs = {0, 1, 2, 3, 4, 5})
    public static int throwCatch3(int a) {

        int b = 1;
        if (a >= 1) b += 1;
        else b += 2;

        try {
            int j = a + 1;
            if (a > 0) throw new Exception();
            b += j;
        } catch (Exception e) {
            return b - 1;
        }
        return b;
    }


    @joptimize.Test(inputs = {0, 1, 2, 3, 4, 5})
    public static int multiCatch(int in) {
        try {
            try {
                try {
                    try {
                        switch (in) {
                            case 0:
                                throw new IOException("IO");
                            case 1:
                                throw new ArrayIndexOutOfBoundsException("Array!");
                            case 2:
                                throw new NullPointerException("NPE");
                            case 3:
                                throw new IllegalArgumentException("IaE");
                            default:
                                throw new Exception("Excc");
                        }
                    } catch (IOException e) {
                        return 0;

                    }
                } catch (ArrayIndexOutOfBoundsException e) {
                    return 1;

                }
            } catch (NullPointerException e) {
                return 2;

            }
        } catch (Exception e) {
            return 3;

        }
    }

    @joptimize.Test(inputs = {0, 1})
    public static String nullPointer(int n) {
        Object o = n == 0 ? null : "";
        try {
            return o.toString();
        } catch (NullPointerException npe) {
            return "null!";
        }
    }

    @joptimize.Test(inputs = {0, 1, 2, 3, 4, 5})
    public static String arrayIndexOutOfBounds(int i) {
        try {
            int[] a = {1, 2, 4, 8};
            return "result! " + a[i];
        } catch (ArrayIndexOutOfBoundsException npe) {
            return npe.getMessage();
        }
    }
}
