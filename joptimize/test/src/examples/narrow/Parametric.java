package joptimize.examples.narrow;

class Parametric {
    @joptimize.Test(
        inputs = {2, 1, 1, 2},
        checkRemoved = {"Parametric.call"},
        checkMangled = {"Parametric.call"}
    )
    static int main(int x, int y) {
        return call(new Bar(), x) + call(new Qux(), y);
        // (x + 1) + (y + 2)
    }

    static <T extends Foo> int call(T f, int n) {
        return f.inc(n);
    }

    @joptimize.Test(
        inputs = {2, 1, 1, 2},
        checkRemoved = {"Parametric.call"},
        checkMangled = {"Parametric.call", "Parametric.callDeep"}
    )
    static int mainDeep(int x, int y) {
        return callDeep(new Bar(), x) + callDeep(new Qux(), y);
        // (x + 1) + (y + 2)
    }

    static <T extends Foo> int callDeep(T f, int n) {
        return call(f, n);
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
