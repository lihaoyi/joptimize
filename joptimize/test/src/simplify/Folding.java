package test.simplify;

class Folding {
    @test.Test(inputs = {1, 2}, addedNumConst = {102})
    static int iadd(int x) {
        int y = 100;
        int z = 2;
        return y + z + x;
    }

    @test.Test(inputs = {1, 2}, addedNumConst = {98})
    static int isub(int x) {
        int y = 100;
        int z = 2;
        return y - z - x;
    }

    @test.Test(inputs = {1, 2}, addedNumConst = {200})
    static int imul(int x) {
        int y = 100;
        int z = 2;
        return y * z * x;
    }

    @test.Test(inputs = {1, 2}, addedNumConst = {50})
    static int idiv(int x) {
        int y = 100;
        int z = 2;
        return y / z / x;
    }

    @test.Test(inputs = {1, 2}, addedNumConst = {0})
    static int irem(int x) {
        int y = 100;
        int z = 2;
        return y % z % x;
    }

    @test.Test(inputs = {1, 2}, addedNumConst = {400})
    static int ishl(int x) {
        int y = 100;
        int z = 2;
        return y << z << x;
    }

    @test.Test(inputs = {1, 2}, addedNumConst = {25})
    static int ishr(int x) {
        int y = 100;
        int z = 2;
        return y >> z >> x;
    }

    @test.Test(inputs = {1, 2}, addedNumConst = {25})
    static int iushr(int x) {
        int y = 100;
        int z = 2;
        return y >>> z >>> x;
    }

    @test.Test(inputs = {1, 2}, addedNumConst = {96})
    static int iand(int x) {
        int y = 100;
        int z = 121;
        return y & z & x;
    }

    @test.Test(inputs = {1, 2}, addedNumConst = {125})
    static int ior(int x) {
        int y = 100;
        int z = 121;
        return y | z | x;
    }

    @test.Test(inputs = {1, 2}, addedNumConst = {29})
    static int ixor(int x) {
        int y = 100;
        int z = 121;
        return y ^ z ^ x;
    }


    @test.Test(inputs = {1, 2}, addedNumConst = {102})
    static long jadd(long x) {
        long y = 100;
        long z = 2;
        return y + z + x;
    }

    @test.Test(inputs = {1, 2}, addedNumConst = {98})
    static long jsub(long x) {
        long y = 100;
        long z = 2;
        return y - z - x;
    }

    @test.Test(inputs = {1, 2}, addedNumConst = {200})
    static long jmul(long x) {
        long y = 100;
        long z = 2;
        return y * z * x;
    }

    @test.Test(inputs = {1, 2}, addedNumConst = {50})
    static long jdiv(long x) {
        long y = 100;
        long z = 2;
        return y / z / x;
    }

    @test.Test(inputs = {1, 2}, addedNumConst = {0})
    static long jrem(long x) {
        long y = 100;
        long z = 2;
        return y % z % x;
    }

    @test.Test(inputs = {1, 2}, addedNumConst = {400})
    static long jshl(long x) {
        long y = 100;
        long z = 2;
        return y << z << x;
    }

    @test.Test(inputs = {1, 2}, addedNumConst = {25})
    static long jshr(long x) {
        long y = 100;
        long z = 2;
        return y >> z >> x;
    }

    @test.Test(inputs = {1, 2}, addedNumConst = {25})
    static long jushr(long x) {
        long y = 100;
        long z = 2;
        return y >>> z >>> x;
    }

    @test.Test(inputs = {1, 2}, addedNumConst = {96})
    static long jand(long x) {
        long y = 100;
        long z = 121;
        return y & z & x;
    }

    @test.Test(inputs = {1, 2}, addedNumConst = {125})
    static long jor(long x) {
        long y = 100;
        long z = 121;
        return y | z | x;
    }

    @test.Test(inputs = {1, 2}, addedNumConst = {29})
    static long jxor(long x) {
        long y = 100;
        long z = 121;
        return y ^ z ^ x;
    }


    @test.Test(inputs = {1, 2}, addedNumConst = {102})
    static float fadd(float x) {
        float y = 100;
        float z = 2;
        return y + z + x;
    }

    @test.Test(inputs = {1, 2}, addedNumConst = {98})
    static float fsub(float x) {
        float y = 100;
        float z = 2;
        return y - z - x;
    }

    @test.Test(inputs = {1, 2}, addedNumConst = {200})
    static float fmul(float x) {
        float y = 100;
        float z = 2;
        return y * z * x;
    }

    @test.Test(inputs = {1, 2}, addedNumConst = {50})
    static float fdiv(float x) {
        float y = 100;
        float z = 2;
        return y / z / x;
    }

    @test.Test(inputs = {1, 2}, addedNumConst = {0})
    static float frem(float x) {
        float y = 100;
        float z = 2;
        return y % z % x;
    }


    @test.Test(inputs = {1, 2}, addedNumConst = {102})
    static double dadd(double x) {
        double y = 100;
        double z = 2;
        return y + z + x;
    }

    @test.Test(inputs = {1, 2}, addedNumConst = {98})
    static double dsub(double x) {
        double y = 100;
        double z = 2;
        return y - z - x;
    }

    @test.Test(inputs = {1, 2}, addedNumConst = {200})
    static double dmul(double x) {
        double y = 100;
        double z = 2;
        return y * z * x;
    }

    @test.Test(inputs = {1, 2}, addedNumConst = {50})
    static double ddiv(double x) {
        double y = 100;
        double z = 2;
        return y / z / x;
    }

    @test.Test(inputs = {1, 2}, addedNumConst = {0})
    static double drem(double x) {
        double y = 100;
        double z = 2;
        return y % z % x;
    }


    @test.Test(inputs = {1, 2}, removedNumConst = {20, 100})
    static boolean lcmp0(int x) {
        long y = 100L;
        long z = 20L;
        return y > z;
    }

    @test.Test(inputs = {1, 2}, removedNumConst = {100})
    static boolean lcmp1(int x) {
        long y = 100L;
        long z = 100L;
        return y > z;
    }

    @test.Test(inputs = {1, 2}, removedNumConst = {200, 100})
    static boolean lcmp2(int x) {
        long y = 100L;
        long z = 200L;
        return y > z;
    }

    @test.Test(inputs = {1, 2}, removedNumConst = {20, 100})
    static boolean fcmp0(int x) {
        float y = 100f;
        float z = 20f;
        return y > z;
    }


    @test.Test(inputs = {1, 2}, removedNumConst = {100})
    static boolean fcmp1(int x) {
        float y = 100f;
        float z = 100f;
        return y > z;
    }

    @test.Test(inputs = {1, 2}, removedNumConst = {200, 100})
    static boolean fcmp2(int x) {
        float y = 100f;
        float z = 200f;
        return y > z;
    }

    @test.Test(inputs = {1, 2}, removedNumConst = {20, 100})
    static boolean dcmp0(int x) {
        double y = 100d;
        double z = 20d;
        return y > z;
    }

    @test.Test(inputs = {1, 2}, removedNumConst = {100})
    static boolean dcmp1(int x) {
        double y = 100d;
        double z = 100d;
        return y > z;
    }

    @test.Test(inputs = {1, 2}, removedNumConst = {200, 100})
    static boolean dcmp2(int x) {
        double y = 100d;
        double z = 200d;
        return y > z;
    }


    @test.Test(inputs = {1, 2}, removedNumConst = {200, 100}, addedNumConst = {102})
    static int jump0(int x) {
        int y = 100;
        int z = 200;
        int w;
        if (y > z) w = 1;
        else w = 2;
        return y + w;
    }

    @test.Test(inputs = {1, 2}, removedNumConst = {200, 100}, addedNumConst = {201})
    static int jump1(int x) {
        int y = 200;
        int z = 100;
        int w;
        if (y > z) w = 1;
        else w = 2;
        return y + w;
    }

    @test.Test(inputs = {1, 2}, removedNumConst = {200, 100}, addedNumConst = {201})
    static long jump2(int x) {
        long y = 200;
        long z = 100;
        int w;
        if (y > z) w = 1;
        else w = 2;
        return y + w;
    }

    @test.Test(inputs = {1, 2}, removedNumConst = {200, 100}, addedNumConst = {102})
    static long jump3(int x) {
        float y = 100f;
        float z = 200;
        int w;
        if (y > z) w = 1;
        else w = 2;
        return (int) (y + w);
    }

    @test.Test(inputs = {1, 2}, removedNumConst = {200, 100}, addedNumConst = {103})
    static long switch0(int x) {
        int y = 100;
        int z = 200;
        int w;
        switch (y + z) {
            case 100:
                w = 1;
                break;
            case 200:
                w = 2;
                break;
            case 300:
                w = 3;
                break;
            default:
                w = 4;
                break;
        }

        return y + w;
    }

    @test.Test(inputs = {1, 2}, removedNumConst = {0, 100}, addedNumConst = {101})
    static long switch1(int x) {
        int y = 100;
        int z = 0;
        int w;
        switch (y + z) {
            case 100:
                w = 1;
                break;
            case 200:
                w = 2;
                break;
            case 300:
                w = 3;
                break;
            default:
                w = 4;
                break;
        }

        return y + w;
    }

    @test.Test(inputs = {1, 2}, removedNumConst = {50, 150}, addedNumConst = {52})
    static long switch2(int x) {
        int y = 50;
        int z = 150;
        int w;
        switch (y + z) {
            case 100:
                w = 1;
                break;
            case 200:
                w = 2;
                break;
            case 300:
                w = 3;
                break;
            default:
                w = 4;
                break;
        }

        return y + w;
    }

    @test.Test(inputs = {1, 2}, removedNumConst = {1000, 0}, addedNumConst = {1004})
    static long switch3(int x) {
        int y = 1000;
        int z = 0;
        int w;
        switch (y + z) {
            case 100:
                w = 1;
                break;
            case 200:
                w = 2;
                break;
            case 300:
                w = 3;
                break;
            default:
                w = 4;
                break;
        }

        return y + w;
    }


}
