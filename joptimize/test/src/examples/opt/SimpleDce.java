package joptimize.examples.opt;

class SimpleDce {
    static int main(int x, int y) {
        return call1(x) + call2(y);
    }

    static int call1(int x) {
        return x + 1;
    }

    static int call2(int y) {
        return y + 2;
    }

    static int call3(int z) {
        return z + 2;
    }
}
