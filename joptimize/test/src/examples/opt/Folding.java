package joptimize.examples.opt;

class Folding {
    @joptimize.Test(inputs = {1, 2}, numConst = {102})
    static int iadd(int x) {
        int y = 100;
        int z = 2;
        return y + z + x;
    }
    @joptimize.Test(inputs = {1, 2}, numConst = {98})
    static int isub(int x) {
        int y = 100;
        int z = 2;
        return y - z - x;
    }
    @joptimize.Test(inputs = {1, 2}, numConst = {200})
    static int imul(int x) {
        int y = 100;
        int z = 2;
        return y * z * x;
    }
    @joptimize.Test(inputs = {1, 2}, numConst = {50})
    static int idiv(int x) {
        int y = 100;
        int z = 2;
        return y / z / x;
    }
    @joptimize.Test(inputs = {1, 2}, numConst = {0})
    static int irem(int x) {
        int y = 100;
        int z = 2;
        return y % z % x;
    }
    @joptimize.Test(inputs = {1, 2}, numConst = {400})
    static int ishl(int x) {
        int y = 100;
        int z = 2;
        return y << z << x;
    }
    @joptimize.Test(inputs = {1, 2}, numConst = {25})
    static int ishr(int x) {
        int y = 100;
        int z = 2;
        return y >> z >> x;
    }
    @joptimize.Test(inputs = {1, 2}, numConst = {25})
    static int iushr(int x) {
        int y = 100;
        int z = 2;
        return y >>> z >>> x;
    }
    
    
    @joptimize.Test(inputs = {1, 2}, numConst = {102})
    static long jadd(long x) {
        long y = 100;
        long z = 2;
        return y + z + x;
    }
    @joptimize.Test(inputs = {1, 2}, numConst = {98})
    static long jsub(long x) {
        long y = 100;
        long z = 2;
        return y - z - x;
    }
    @joptimize.Test(inputs = {1, 2}, numConst = {200})
    static long jmul(long x) {
        long y = 100;
        long z = 2;
        return y * z * x;
    }
    @joptimize.Test(inputs = {1, 2}, numConst = {50})
    static long jdiv(long x) {
        long y = 100;
        long z = 2;
        return y / z / x;
    }
    @joptimize.Test(inputs = {1, 2}, numConst = {0})
    static long jrem(long x) {
        long y = 100;
        long z = 2;
        return y % z % x;
    }
    @joptimize.Test(inputs = {1, 2}, numConst = {400})
    static long jshl(long x) {
        long y = 100;
        long z = 2;
        return y << z << x;
    }
    @joptimize.Test(inputs = {1, 2}, numConst = {25})
    static long jshr(long x) {
        long y = 100;
        long z = 2;
        return y >> z >> x;
    }
    @joptimize.Test(inputs = {1, 2}, numConst = {25})
    static long jushr(long x) {
        long y = 100;
        long z = 2;
        return y >>> z >>> x;
    }
    
    
    @joptimize.Test(inputs = {1, 2}, numConst = {102})
    static float fadd(float x) {
        float y = 100;
        float z = 2;
        return y + z + x;
    }
    @joptimize.Test(inputs = {1, 2}, numConst = {98})
    static float fsub(float x) {
        float y = 100;
        float z = 2;
        return y - z - x;
    }
    @joptimize.Test(inputs = {1, 2}, numConst = {200})
    static float fmul(float x) {
        float y = 100;
        float z = 2;
        return y * z * x;
    }
    @joptimize.Test(inputs = {1, 2}, numConst = {50})
    static float fdiv(float x) {
        float y = 100;
        float z = 2;
        return y / z / x;
    }
    @joptimize.Test(inputs = {1, 2}, numConst = {0})
    static float frem(float x) {
        float y = 100;
        float z = 2;
        return y % z % x;
    }
    
    
    @joptimize.Test(inputs = {1, 2}, numConst = {102})
    static double dadd(double x) {
        double y = 100;
        double z = 2;
        return y + z + x;
    }
    @joptimize.Test(inputs = {1, 2}, numConst = {98})
    static double dsub(double x) {
        double y = 100;
        double z = 2;
        return y - z - x;
    }
    @joptimize.Test(inputs = {1, 2}, numConst = {200})
    static double dmul(double x) {
        double y = 100;
        double z = 2;
        return y * z * x;
    }
    @joptimize.Test(inputs = {1, 2}, numConst = {50})
    static double ddiv(double x) {
        double y = 100;
        double z = 2;
        return y / z / x;
    }
    @joptimize.Test(inputs = {1, 2}, numConst = {0})
    static double drem(double x) {
        double y = 100;
        double z = 2;
        return y % z % x;
    }
}
