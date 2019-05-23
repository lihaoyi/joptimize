package test.simplify;

class InstanceDce {
    static FooTwo unknown1 = new BarTwo();
    static FooTwo unknown2 = new QuxTwo();

    @test.Test(
            inputs = {1, 2},
            checkPresent = {"BarTwo.incA", "QuxTwo.incB"},
            checkRemoved = {"BarTwo.incB", "QuxTwo.incA"}
    )
    static int simple1(int x, int y) {
        return new BarTwo().incA(x) + new QuxTwo().incB(y);
    }

    @test.Test(
            inputs = {1, 2},
            checkPresent = {"BarTwo.incA", "BarTwo.incB"},
            checkClassRemoved = {"QuxTwo"}
    )
    static int simple2(int x, int y) {
        return new BarTwo().incA(x) + new BarTwo().incB(y);
    }

    @test.Test(
            inputs = {1, 2},
            checkPresent = {"QuxTwo.incA", "QuxTwo.incB"},
            checkClassRemoved = {"BarTwo"}
    )
    static int simple3(int x, int y) {
        return new QuxTwo().incA(x) + new QuxTwo().incB(y);
    }

    @test.Test(
            inputs = {1, 2},
            checkPresent = {"BarTwo.incA"},
            checkRemoved = {"BarTwo.incB"},
            checkClassRemoved = {"QuxTwo"}
    )
    static int single1(int x, int y) {
        return new BarTwo().incA(x) + new BarTwo().incA(y);
    }

    @test.Test(
            inputs = {1, 2},
            checkPresent = {"BarTwo.incB"},
            checkRemoved = {"BarTwo.incA"},
            checkClassRemoved = {"QuxTwo"}
    )
    static int single2(int x, int y) {
        return new BarTwo().incB(x) + new BarTwo().incB(y);
    }

    @test.Test(
            inputs = {1, 2},
            checkPresent = {"QuxTwo.incA"},
            checkRemoved = {"QuxTwo.incB"},
            checkClassRemoved = {"BarTwo"}
    )
    static int single3(int x, int y) {
        return new QuxTwo().incA(x) + new QuxTwo().incA(y);
    }


    @test.Test(
            inputs = {1, 2},
            checkPresent = {"QuxTwo.incB"},
            checkRemoved = {"QuxTwo.incA"},
            checkClassRemoved = {"BarTwo"}
    )
    static int single4(int x, int y) {
        return new QuxTwo().incB(x) + new QuxTwo().incB(y);
    }

    @test.Test(
            inputs = {1, 2},
            checkPresent = {"BarTwo.incA", "QuxTwo.incA", "FooTwo.incA"},
            checkRemoved = {"BarTwo.incB", "QuxTwo.incB", "FooTwo.incB"}
    )
    static int unknown1(int x, int y) {
        return unknown1.incA(x) + unknown2.incA(y);
    }

    @test.Test(
            inputs = {1, 2},
            checkPresent = {"BarTwo.incB", "QuxTwo.incB", "FooTwo.incB"},
            checkRemoved = {"BarTwo.incA", "QuxTwo.incA", "FooTwo.incA"}
    )
    static int unknown2(int x, int y) {
        return unknown1.incB(x) + unknown2.incB(y);
    }


    @test.Test(
            inputs = {1, 2},
            checkPresent = {
                    "BarTwo.incA", "BarTwo.incB",
                    "QuxTwo.incA", "QuxTwo.incB",
                    "FooTwo.incA", "FooTwo.incB"
            }
    )
    static int unknown3(int x, int y) {
        return unknown1.incA(x) + unknown2.incB(y);
    }

}

interface FooTwo {
    int incA(int n);

    int incB(int n);
}

class BarTwo implements FooTwo {
    public int incA(int n) {
        return n + 1;
    }

    public int incB(int n) {
        return n + 2;
    }
}

class QuxTwo implements FooTwo {
    public int incA(int n) {
        return n + 3;
    }

    public int incB(int n) {
        return n + 4;
    }
}