package joptimize.examples.simplify;

class InterfacePreservation {
    @joptimize.Test(
        inputs = {1, 2}
    )
    static int shallow(int i) {
        return new Bar().run(i);
    }

    @joptimize.Test(
        inputs = {1, 2}
    )
    static int deep(int i) {
        return new Qux().run(i);
    }

    interface Foo {
    }

    static class Bar implements Foo {
        int run(int i) {
            return i + 1;
        }
    }

    interface Baz extends Foo {
    }

    static class Qux implements Baz {
        int run(int i) {
            return i + 2;
        }
    }
}

