package test.simple;

public class Arrays {
    @test.Test(inputs = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12})
    public static int length(int n) {
        int[] x = new int[n + 1];
        return x.length;
    }

    @test.Test(inputs = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12})
    public static int get1(int n) {
        int[] x = new int[n + 1];
        return x[n];
    }

    @test.Test(inputs = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12})
    public static int get2(int n) {
        int[][] x = new int[n + 1][n + 1];
        return x[n][n];
    }

    @test.Test(inputs = {0, 1, 2})
    public static int get3(int n) {
        int[] x = {5, 7, 9};
        return x[n];
    }

    @test.Test(inputs = {0, 0})
    public static int get4(int n, int m) {
        int[][] x = {{1337}};
        return x[n][m];
    }

    @test.Test(inputs = {0, 0, 0, 1, 1, 1, 1, 0})
    public static int get5(int n, int m) {
        int[][] x = {{1, 2}, {4, 5}};
        return x[n][m];
    }

    @test.Test(inputs = {1, 2, 3, 4, 5, 6})
    public static int[] set1(int a) {
        int[] x = new int[a];
        x[0] = 999;
        return x;
    }

    @test.Test(inputs = {1, 2, 3, 4, 5, 6})
    public static int[][] set2(int a, int b) {
        int[][] x = new int[a][b];
        x[0][0] = 999;
        return x;
    }

    @test.Test(inputs = {1, 2, 3, 4, 5, 6})
    public static double[][][] set3(int a, int b, int c) {
        double[][][] x = new double[a][b][c];
        x[a - 1][b - 1][c - 1] = 999;
        return x;
    }

    @test.Test(inputs = {1, 2, 3, 4, 5, 6})
    public static int[] setIf(int a) {
        int[] x = new int[a];
        if (a > 4) x[0] = 999;
        else x[0] = 777;
        return x;
    }

    @test.Test(inputs = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12})
    public static int getAndSet1(int n) {
        int[] x = new int[n + 1];
        int y = x[n];
        x[n] = y + 1;
        return x[n];
    }

    @test.Test(inputs = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12})
    public static int getAndSet2(int n) {
        int[] x = new int[n + 1];
        int y = x[n];
        x[n] = y + 1;
        int z = x[n];
        x[n] = z + 1;
        return x[n];
    }

    @test.Test(inputs = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12})
    public static int getAndSet3(int n) {
        int[][] x = new int[n + 1][n + 1];
        int y = x[n - 1][n];
        x[n - 1][n] = y + 1;
        int z = x[n - 1][n];
        x[n - 1][n] = z + 1;
        return x[n - 1][n];
    }

    @test.Test(inputs = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12})
    public static int[] getAndSetLoop1(int n) {
        int[] arr = new int[n];
        for (int i = 0; i < n; i++) {
            arr[i] = i;
        }

        return arr;
    }

    @test.Test(inputs = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12})
    public static int getAndSetLoop2(int n) {
        int[] arr = new int[n];

        int total = 0;
        for (int i = 0; i < n; i++) {
            total += arr[i];
        }
        return total;
    }

    @test.Test(inputs = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12})
    public static int getAndSetLoop3(int n) {
        int[][] arr = new int[n][n];
        for (int i = 0; i < n; i++) {
            for (int j = 0; j < n; j++) {
                arr[i][j] = i + j;
            }
        }
        int total = 0;
        for (int i = 0; i < n; i++) {
            for (int j = 0; j < n; j++) {
                total += arr[i][j];
            }
        }
        return total;
    }
}