package test.inlining;

public class Inlining {
    @test.Test(
            inputs = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12},
            checkRemoved = {"Inlining.called"},
            inline = true
    )
    static int simple(int a){
        return called(a);
    }

    static int called(int a) {
        return a + 1;
    }

    @test.Test(
            inputs = {0, 0, 0, 1, 1, 0, 1, 1},
            inline = true
    )
    static int conditional(int a, int b) {
        return a == 0 ? conditional0(b) : -1;
    }

    static int conditional0(int bc) {
        return bc != 0 ? 1 : 0 ;
    }

    @test.Test(
            inputs = {0, 0, 0, 1, 1, 0, 1, 1},
            inline = true
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
            inline = true
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
}
