package test.narrow;

class IntersectionReturn {
    @test.Test(inputs = {2, 1, 1, 2})
    static int main(int x, int y) {
        return call(x, y) instanceof Bar ? 1 : 2;
    }

    static Foo call(int x, int y) {
        return x > y ? new Bar() : new Qux();
    }

    interface Foo {
        int inc(int n);
    }

    interface Foo1 extends Foo {
    }

    interface Foo2 extends Foo {
    }

    static class Bar implements Foo1, Foo2 {
        public int inc(int n) {
            return n + 1;
        }
    }

    static class Qux implements Foo1, Foo2 {
        public int inc(int n) {
            return n + 2;
        }
    }
}

