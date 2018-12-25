package joptimize.examples.narrow;

class MergeReturn {
    @joptimize.Test(inputs = {2, 1, 1, 2})
    static int main(int x, int y) {
        return call(x, y) instanceof Bar ? 1 : 2;
    }

    static Foo call(int x, int y) {
        if (x > y) return new Bar();
        else return new Qux();
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
