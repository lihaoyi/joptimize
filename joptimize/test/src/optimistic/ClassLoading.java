package test.optimistic;

class ClassLoading {
    @test.Test(
            // Make sure that in this case, since `Bar` is the only subtype
            // instantiated, we can constant fold `1 + 123` into `124`
            removedNumConst = {123},
            addedNumConst = {124}
    )
    public static int monomorphicOptimize() {
        Foo foo = new Bar();
        int first = foo.call() + 123;
        return first;
    }


    @test.Test(
            // These numbers should *not* get constant folded, as there are two
            // subtypes of `Foo` and we do not know which one `.call` is called on
            addedNumConst = {123}
    )
    public static int preBimorphicUnoptimize() {
        Foo foo = new Bar();
        foo = new Qux();
        int first = foo.call() + 123;
        return first;
    }


    @test.Test(
            // These numbers should *not* get constant folded, as there are two
            // subtypes of `Foo` and we do not know which one `.call` is called on
            addedNumConst = {123}
    )
    public static int postBimorphicDeoptimize() {
        Foo foo = new Bar();
        int first = foo.call() + 123;
        foo = new Qux();
        return first;
    }


    @test.Test(
            // These numbers should *not* get constant folded, as there are two
            // subtypes of `Foo` and we do not know which one `.call` is called on
            addedNumConst = {123, 456}
    )
    public static int prePostBimorphicDeoptimize() {
        Foo foo = new Bar();
        int first = foo.call() + 123;
        foo = new Qux();
        int second = foo.call() + 456;
        return first + second;
    }
    static abstract class Foo{
        abstract int call();
    }
    static class Bar extends Foo{
        int call(){
            return 1;
        }
    }
    static class Qux extends Foo{
        int call(){
            return 2;
        }
    }
}
