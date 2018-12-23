package joptimize.examples.narrow;

class Supertype {
    static int main(int x, int y) {
        return call(new Bar(), x) + call(new Qux(), y);
        // (x + 1) + (y + 2)
    }

    static int call(Foo f, int n) {
        return f.inc(n);
    }

    static int mainDeep(int x, int y) {
        return callDeep(new Bar(), x) + callDeep(new Qux(), y);
        // (x + 1) + (y + 2)
    }

    static int callDeep(Foo f, int n) {
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