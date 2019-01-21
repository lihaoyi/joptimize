package joptimize.examples.simple;

public class MultiDimArrays {
    @joptimize.Test(inputs = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12})
    public static int get1(int n) {
        int[] x = new int[n + 1];
        return x[n];
    }

    @joptimize.Test(inputs = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12})
    public static int get2(int n) {
        int[][] x = new int[n + 1][n + 1];
        return x[n][n];
    }

    @joptimize.Test(inputs = {1, 2, 3, 4, 5, 6})
    public static int[] set1(int a) {
        int[] x = new int[a];
        x[0] = 999;
        return x;
    }

    @joptimize.Test(inputs = {1, 2, 3, 4, 5, 6})
    public static int[][] set2(int a, int b) {
        int[][] x = new int[a][b];
        x[0][0] = 999;
        return x;
    }

    @joptimize.Test(inputs = {1, 2, 3, 4, 5, 6})
    public static double[][][] set3(int a, int b, int c) {
        double[][][] x = new double[a][b][c];
        x[a-1][b-1][c-1] = 999;
        return x;
    }

    @joptimize.Test(inputs = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12})
    public static int getAndSet1(int n) {
        int[] x = new int[n + 1];
        int y = x[n];
        x[n] = y + 1;
        return x[n];
    }
    @joptimize.Test(inputs = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12})
    public static int getAndSet2(int n) {
        int[] x = new int[n + 1];
        int y = x[n];
        x[n] = y + 1;
        int z = x[n];
        x[n] = z + 1;
        return x[n];
    }
    @joptimize.Test(inputs = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12})
    public static int getAndSet3(int n) {
        int[][] x = new int[n + 1][n + 1];
        int y = x[n - 1][n];
        x[n - 1][n] = y + 1;
        int z = x[n - 1][n];
        x[n - 1][n] = z + 1;
        return x[n - 1][n];
    }

    @joptimize.Test(inputs = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12})
    public static int getAndSetLoop(int n) {
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