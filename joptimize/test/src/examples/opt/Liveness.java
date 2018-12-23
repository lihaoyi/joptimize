package joptimize.examples.opt;

public class Liveness {
    static int main(int i) {
        pureButNotConstant(i);
        return i + 1;
    }

    static int pureButNotConstant(int i) {
        return i - 1;
    }

    static int pureButNotConstant2(int i) {
        return i + 1;
    }


    static int main2a(int i) {
        return terminal(true, pureButNotConstant(i), pureButNotConstant2(i));
    }

    static int main2b(int i) {
        return terminal(false, pureButNotConstant(i), pureButNotConstant2(i));
    }

    static int terminal(boolean b, int i, int j) {
        return b ? i : j;
    }
}