package joptimize.examples.opt;

class Folding {
    @joptimize.Test(inputs = {1, 2}, addedNumConst = {102})
    static int iadd(int x) {
        int y = 100;
        int z = 2;
        return y + z + x;
    }
    @joptimize.Test(inputs = {1, 2}, addedNumConst = {98})
    static int isub(int x) {
        int y = 100;
        int z = 2;
        return y - z - x;
    }
    @joptimize.Test(inputs = {1, 2}, addedNumConst = {200})
    static int imul(int x) {
        int y = 100;
        int z = 2;
        return y * z * x;
    }
    @joptimize.Test(inputs = {1, 2}, addedNumConst = {50})
    static int idiv(int x) {
        int y = 100;
        int z = 2;
        return y / z / x;
    }
    @joptimize.Test(inputs = {1, 2}, addedNumConst = {0})
    static int irem(int x) {
        int y = 100;
        int z = 2;
        return y % z % x;
    }
    @joptimize.Test(inputs = {1, 2}, addedNumConst = {400})
    static int ishl(int x) {
        int y = 100;
        int z = 2;
        return y << z << x;
    }
    @joptimize.Test(inputs = {1, 2}, addedNumConst = {25})
    static int ishr(int x) {
        int y = 100;
        int z = 2;
        return y >> z >> x;
    }
    @joptimize.Test(inputs = {1, 2}, addedNumConst = {25})
    static int iushr(int x) {
        int y = 100;
        int z = 2;
        return y >>> z >>> x;
    }
    @joptimize.Test(inputs = {1, 2}, addedNumConst = {96})
    static int iand(int x) {
        int y = 100;
        int z = 121;
        return y & z & x;
    }
    @joptimize.Test(inputs = {1, 2}, addedNumConst = {125})
    static int ior(int x) {
        int y = 100;
        int z = 121;
        return y | z | x;
    }
    @joptimize.Test(inputs = {1, 2}, addedNumConst = {29})
    static int ixor(int x) {
        int y = 100;
        int z = 121;
        return y ^ z ^ x;
    }
    
    
    @joptimize.Test(inputs = {1, 2}, addedNumConst = {102})
    static long jadd(long x) {
        long y = 100;
        long z = 2;
        return y + z + x;
    }
    @joptimize.Test(inputs = {1, 2}, addedNumConst = {98})
    static long jsub(long x) {
        long y = 100;
        long z = 2;
        return y - z - x;
    }
    @joptimize.Test(inputs = {1, 2}, addedNumConst = {200})
    static long jmul(long x) {
        long y = 100;
        long z = 2;
        return y * z * x;
    }
    @joptimize.Test(inputs = {1, 2}, addedNumConst = {50})
    static long jdiv(long x) {
        long y = 100;
        long z = 2;
        return y / z / x;
    }
    @joptimize.Test(inputs = {1, 2}, addedNumConst = {0})
    static long jrem(long x) {
        long y = 100;
        long z = 2;
        return y % z % x;
    }
    @joptimize.Test(inputs = {1, 2}, addedNumConst = {400})
    static long jshl(long x) {
        long y = 100;
        long z = 2;
        return y << z << x;
    }
    @joptimize.Test(inputs = {1, 2}, addedNumConst = {25})
    static long jshr(long x) {
        long y = 100;
        long z = 2;
        return y >> z >> x;
    }
    @joptimize.Test(inputs = {1, 2}, addedNumConst = {25})
    static long jushr(long x) {
        long y = 100;
        long z = 2;
        return y >>> z >>> x;
    }
    @joptimize.Test(inputs = {1, 2}, addedNumConst = {96})
    static long jand(long x) {
        long y = 100;
        long z = 121;
        return y & z & x;
    }
    @joptimize.Test(inputs = {1, 2}, addedNumConst = {125})
    static long jor(long x) {
        long y = 100;
        long z = 121;
        return y | z | x;
    }
    @joptimize.Test(inputs = {1, 2}, addedNumConst = {29})
    static long jxor(long x) {
        long y = 100;
        long z = 121;
        return y ^ z ^ x;
    }




    @joptimize.Test(inputs = {1, 2}, addedNumConst = {102})
    static float fadd(float x) {
        float y = 100;
        float z = 2;
        return y + z + x;
    }
    @joptimize.Test(inputs = {1, 2}, addedNumConst = {98})
    static float fsub(float x) {
        float y = 100;
        float z = 2;
        return y - z - x;
    }
    @joptimize.Test(inputs = {1, 2}, addedNumConst = {200})
    static float fmul(float x) {
        float y = 100;
        float z = 2;
        return y * z * x;
    }
    @joptimize.Test(inputs = {1, 2}, addedNumConst = {50})
    static float fdiv(float x) {
        float y = 100;
        float z = 2;
        return y / z / x;
    }
    @joptimize.Test(inputs = {1, 2}, addedNumConst = {0})
    static float frem(float x) {
        float y = 100;
        float z = 2;
        return y % z % x;
    }
    
    
    @joptimize.Test(inputs = {1, 2}, addedNumConst = {102})
    static double dadd(double x) {
        double y = 100;
        double z = 2;
        return y + z + x;
    }
    @joptimize.Test(inputs = {1, 2}, addedNumConst = {98})
    static double dsub(double x) {
        double y = 100;
        double z = 2;
        return y - z - x;
    }
    @joptimize.Test(inputs = {1, 2}, addedNumConst = {200})
    static double dmul(double x) {
        double y = 100;
        double z = 2;
        return y * z * x;
    }
    @joptimize.Test(inputs = {1, 2}, addedNumConst = {50})
    static double ddiv(double x) {
        double y = 100;
        double z = 2;
        return y / z / x;
    }
    @joptimize.Test(inputs = {1, 2}, addedNumConst = {0})
    static double drem(double x) {
        double y = 100;
        double z = 2;
        return y % z % x;
    }


    @joptimize.Test(inputs = {1, 2}, removedNumConst = {20, 100})
    static boolean lcmp0(int x) {
        long y = 100L;
        long z = 20L;
        return y > z;
    }

    @joptimize.Test(inputs = {1, 2}, removedNumConst = {100})
    static boolean lcmp1(int x) {
        long y = 100L;
        long z = 100L;
        return y > z;
    }

    @joptimize.Test(inputs = {1, 2}, removedNumConst = {200, 100})
    static boolean lcmp2(int x) {
        long y = 100L;
        long z = 200L;
        return y > z;
    }

    @joptimize.Test(inputs = {1, 2}, removedNumConst = {20, 100})
    static boolean fcmp0(int x) {
        float y = 100f;
        float z = 20f;
        return y > z;
    }


    @joptimize.Test(inputs = {1, 2}, removedNumConst = {100})
    static boolean fcmp1(int x) {
        float y = 100f;
        float z = 100f;
        return y > z;
    }
    @joptimize.Test(inputs = {1, 2}, removedNumConst = {200, 100})
    static boolean fcmp2(int x) {
        float y = 100f;
        float z = 200f;
        return y > z;
    }

    @joptimize.Test(inputs = {1, 2}, removedNumConst = {20, 100})
    static boolean dcmp0(int x) {
        double y = 100d;
        double z = 20d;
        return y > z;
    }
    @joptimize.Test(inputs = {1, 2}, removedNumConst = {100})
    static boolean dcmp1(int x) {
        double y = 100d;
        double z = 100d;
        return y > z;
    }
    @joptimize.Test(inputs = {1, 2}, removedNumConst = {200, 100})
    static boolean dcmp2(int x) {
        double y = 100d;
        double z = 200d;
        return y > z;
    }
}
