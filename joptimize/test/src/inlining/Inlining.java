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
}
