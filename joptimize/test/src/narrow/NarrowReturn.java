package test.narrow;

class NarrowReturn {
    @test.Test(
            inputs = {2, 1, 1, 2},
            checkRemoved = {"NarrowReturn.call"},
            checkMangled = {"NarrowReturn.call"}
    )
    static int main(int x, int y) {
        return call(wrap(new Bar()), x) + call(wrap(new Qux()), y);
    }

    static int call(Foo f, int n) {
        return f.inc(n);
    }

    static Foo wrap(Foo f) {
        return f;
    }

    interface Foo {
        int inc(int n);
    }

    static class Bar implements Foo {
        public int inc(int n) {
            return n + 1;
        }
    }

    static class Qux implements Foo {
        public int inc(int n) {
            return n + 2;
        }
    }
}
