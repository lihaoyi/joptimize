package test.narrow;

class ForceWide {
    static Foo f1 = new Bar();
    static Foo f2 = new Qux();

    @test.Test(
        inputs = {1, 2},
        checkPresent = {"ForceWide.call"}
    )
    static int main(int x, int y) {
        return call(f1, x) + call(f2, y);
        // (x + 1) + (y + 2)
    }

    static int call(Foo f, int n) {
        return f.inc(n);
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

