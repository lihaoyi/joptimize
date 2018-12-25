package joptimize.examples.simple;

public class MultiDimArrays {
    @joptimize.Test(inputs = {1, 2, 3, 4, 5, 6})
    public static int[][] make2D(int a, int b) {
        return new int[a][b];
    }

    @joptimize.Test(inputs = {1, 2, 3, 4, 5, 6})
    public static int[][][] make3D(int a, int b, int c) {
        return new int[a][b][c];
    }

    @joptimize.Test(inputs = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12})
    public static int getAndSet(int n) {
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