package test.inlining;

public class Inlining {
    @test.Test(
            inputs = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12},
            checkRemoved = {"Inlining.called"},
            assertOnInlined = true
    )
    static int simple(int a){
        return called(a);
    }

    static int called(int a) {
        return a + 1;
    }

    @test.Test(
            inputs = {0, 0, 0, 1, 1, 0, 1, 1},
            checkRemoved = {"Inlining.conditional0"},
            assertOnInlined = true
    )
    static int conditional(int a, int b) {
        return a == 0 ? conditional0(b) : -1;
    }

    static int conditional0(int bc) {
        return bc != 0 ? 1 : 0 ;
    }

    @test.Test(
            inputs = {0, 0, 0, 1, 1, 0, 1, 1},
            checkRemoved = {"Inlining.conditionalIf0"},
            assertOnInlined = true
    )
    static int conditionalIf(int a, int b) {
        if (a == 0){
            return conditionalIf0(b);
        }else{
            return 0;
        }
    }

    static int conditionalIf0(int bc) {
        if (bc == 0){
            return 1;
        }else{
            return -1;
        }
    }
    @test.Test(
            inputs = {0, 0, 0, 1, 1, 0, 1, 1},
            checkRemoved = {"Inlining.conditionalVoid0"},
            assertOnInlined = true
    )
    static int conditionalVoid(int a, int b) {
        int[] box = {0};
        if (a == 0){
            conditionalVoid0(box, b);
        }
        return box[0];
    }

    static void conditionalVoid0(int[] box, int bc) {
        if (bc == 0){
            box[0] = bc;
        }else{
            box[0] = -bc;
        }
        return;
    }
    @test.Test(
            inputs = {0, 0, 0, 1, 1, 0, 1, 1},
            checkRemoved = {"Inlining.conditionalIfVoid0"},
            assertOnInlined = true
    )
    static int conditionalIfVoid(int a, int b) {
        int[] box = {0};
        if (a == 0){
            conditionalIfVoid0(box, b);
            return box[0];
        }else{
            return box[0];
        }
    }

    static void conditionalIfVoid0(int[] box, int bc) {
        if (bc == 0){
            box[0] = bc;
            return;
        }else{
            box[0] = -bc;
            return;
        }
    }
    @test.Test(
            inputs = {0},
            checkRemoved = {"Inlining.thrower0"},
            assertOnInlined = true
    )
    static int thrower(int a) throws Exception {
        if (a != 0){
            thrower0();
            return (a + 123) * 456;
        }else{
            return 789;
        }
    }

    static void thrower0() throws Exception{
        throw new Exception();
    }
    @test.Test(
            inputs = {0},
            checkRemoved = {"Inlining.throwerComplex0"},
            assertOnInlined = true
    )
    static int throwerComplex(int a) throws Exception {
        if (a > 10) {
            throwerComplex0(a);
            if (a > 100) return 123;
            else return 456;
        }
        return 1;
    }

    static void throwerComplex0(int a) throws Exception{
        int x = 1;
        if (a > 100) x += 2;
        throw new Exception(String.valueOf(x));
    }
    @test.Test(
            inputs = {0},
            checkRemoved = {"Inlining.throwerPartialMerge0"},
            assertOnInlined = true
    )
    static int throwerPartialMerge(int a) throws Exception {
        int x = 0;
        if (a != 0){
            throwerPartialMerge0(a);
            x = 1;
        }else{
            x = 2;
        }
        return x;
    }

    static void throwerPartialMerge0(int a) throws Exception{
        int x = 1;
        if (a > 100) x += 2;
        else x += 3;
        throw new Exception(String.valueOf(x));
    }
    @test.Test(
            inputs = {0},
            checkRemoved = {"Inlining.throwerFullMerge1", "Inlining.throwerFullMerge2"},
            assertOnInlined = true
    )
    static int throwerFullMerge(int a) throws Exception {
        int x = 0;
        if (a > 100){
            if (a > 200){
                throwerFullMerge1(a);
                x = 1;
            }else{
                throwerFullMerge2(a);
                x = 2;
            }
            return x;
        } else{
            return -1;
        }
    }

    static void throwerFullMerge1(int a) throws Exception{
        int x = 1;
        if (a > 100) x += 1;
        else x += 3;
        throw new Exception(String.valueOf(x));
    }
    static void throwerFullMerge2(int a) throws Exception{
        int x = 1;
        if (a > 100) x += 2;
        else x += 3;
        throw new Exception(String.valueOf(x));
    }
}
