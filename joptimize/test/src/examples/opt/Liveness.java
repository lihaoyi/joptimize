package joptimize.examples.opt;

class Liveness {

    @joptimize.Test(
        inputs = {1, 2, 3, 4, 5, 6, 7, 8}
    )
    static int entrypointUnused0(int i) {
        return 123;
    }

    @joptimize.Test(
        inputs = {1, 2, 3, 4, 5, 6, 7, 8}
    )
    static int entrypointUnused1(int i, int j) {
        return 123 + j;

    }

    @joptimize.Test(
        inputs = {1, 2, 3, 4, 5, 6, 7, 8}
    )
    static int entrypointUnused2(int i, int j) {
        return 123 + i;

    }

    @joptimize.Test(
            inputs = {1, 2},
            removedNumConst = {456},
            addedNumConst = {123}
    )
    static int trivialUnused(int i) {
        return trivialUnused0(i + 123, i + 456);

    }

    static int trivialUnused0(int i, int j) {
        return i;
    }


    @joptimize.Test(
        inputs = {1, 2},
        checkPresent = {"Liveness.simple"},
        checkRemoved = {"Liveness.pureButNotConstant"}
    )
    static int simple(int i) {
        pureButNotConstant(i);
        return i + 1;
    }

    static int pureButNotConstant(int i) {
        return i - 1;
    }

    static int pureButNotConstant2(int i) {
        return i + 1;
    }

    @joptimize.Test(
        inputs = {1, 2},
        checkPresent = {"Liveness.simple2a", "Liveness.pureButNotConstant"},
        checkRemoved = {"Liveness.pureButNotConstant2"}
    )
    static int simple2a(int i) {
        return terminal(true, pureButNotConstant(i), pureButNotConstant2(i));
    }


    @joptimize.Test(
        inputs = {1, 2},
        checkPresent = {"Liveness.simple2b", "Liveness.pureButNotConstant2"},
        checkRemoved = {"Liveness.pureButNotConstant"}
    )
    static int simple2b(int i) {
        return terminal(false, pureButNotConstant(i), pureButNotConstant2(i));
    }

    static int terminal(boolean b, int i, int j) {
        return b ? i : j;
    }

    @joptimize.Test(
            inputs = {1, 2},
            checkPresent = {"Liveness.chained"},
            checkRemoved = {"Liveness.pureButNotConstant"}
    )
    static int chained(int i) {
        pureButNotConstant(((i * i) * 2) - i);
        return i + 1;
    }

    @joptimize.Test(
            inputs = {1, 2},
            checkPresent = {"Liveness.chained2a", "Liveness.pureButNotConstant"},
            checkRemoved = {"Liveness.pureButNotConstant2"}
    )
    static int chained2a(int i) {
        return terminal(true, pureButNotConstant(i), pureButNotConstant2(i * 2));
    }

    @joptimize.Test(
            inputs = {1, 2},
            checkPresent = {"Liveness.chained2b", "Liveness.pureButNotConstant2"},
            checkRemoved = {"Liveness.pureButNotConstant"}
    )
    static int chained2b(int i) {
        return terminal(false, pureButNotConstant((i - 1) * (i - 5)), pureButNotConstant2(i));
    }


    @joptimize.Test(
            inputs = {1, 2, 3, 4, 5, 6, 7, 8},
            removedNumConst = {123},
            addedNumConst = {456}
    )
    static int implement0(int i) {
        return (new Bar0()).choose(i + 123, i + 456);
    }
    static abstract class Foo0{
        abstract int choose(int x, int y);
    }
    static class Bar0 extends Foo0{
        int choose(int x, int y){
            return y;
        }
    }


    @joptimize.Test(
            inputs = {1, 2, 3, 4, 5, 6, 7, 8},
            removedNumConst = {456},
            addedNumConst = {123}
    )
    static int implement1(int i) {
        return (new Bar1()).choose(i + 123, i + 456);
    }
    static abstract class Foo1{
        abstract int choose(int x, int y);
    }
    static class Bar1 extends Foo1{
        int choose(int x, int y){
            return x;
        }
    }


    @joptimize.Test(
            inputs = {1, 2, 3, 4, 5, 6, 7, 8}
    )
    static int implement2a(int i) {
        return (new Bar2()).choose(i + 123, i + 456);
    }
    @joptimize.Test(
            inputs = {1, 2, 3, 4, 5, 6, 7, 8},
            addedNumConst = {123, 456}
    )
    static int implement2b(int i) {
        Foo2 bar = new Bar2();
        bar.choose(i + 123, i + 456);
        Foo2 qux = new Qux2();
        return qux.choose(i + 123, i + 456);
    }
    static abstract class Foo2{
        abstract int choose(int x, int y);
    }
    static class Bar2 extends Foo2{
        int choose(int x, int y){
            return x;
        }
    }
    static class Qux2 extends Foo2{
        int choose(int x, int y){
            return y;
        }
    }


    @joptimize.Test(
            inputs = {1, 2, 3, 4, 5, 6, 7, 8},
            addedNumConst = {123, 456}
    )
    static int implement3(int i) {
        Foo2 bar = new Bar2();
        bar.choose(i + 123, i + 456);
        Foo2 qux = new Qux2();
        return qux.choose(i + 123, i + 456);
    }
    interface Foo3{
        int choose(int x, int y);
    }
    static class Bar3 implements Foo3{
        public int choose(int x, int y){
            return x;
        }
    }
    static class Qux3 implements Foo3{
        public int choose(int x, int y){
            return y;
        }
    }


    @joptimize.Test(
            inputs = {1, 2, 3, 4, 5, 6, 7, 8},
            removedNumConst = {123},
            addedNumConst = {456}
    )
    static int override0(int i) {
        return (new BarOverride0()).choose(i + 123, i + 456);
    }
    static abstract class FooOverride0{
        int choose(int x, int y){
            return y;
        }
    }
    static class BarOverride0 extends FooOverride0{
        int choose(int x, int y){
            return y;
        }
    }


    @joptimize.Test(
            inputs = {1, 2, 3, 4, 5, 6, 7, 8},
            removedNumConst = {456},
            addedNumConst = {123}
    )
    static int override1(int i) {
        return (new BarOverride1()).choose(i + 123, i + 456);
    }
    static abstract class FooOverride1{
        int choose(int x, int y){
            return x;
        }
    }
    static class BarOverride1 extends FooOverride1{
        int choose(int x, int y){
            return x;
        }
    }


    @joptimize.Test(
            inputs = {1, 2, 3, 4, 5, 6, 7, 8},
            addedNumConst = {123, 456}
    )
    static int override2(int i) {
        FooOverride2 x = new BarOverride2();
        return x.choose(i + 123, i + 456);
    }
    static abstract class FooOverride2{
        int choose(int x, int y){
            return y;
        }
    }
    static class BarOverride2 extends FooOverride2{
        int choose(int x, int y){
            return x;
        }
    }



    @joptimize.Test(
            inputs = {1, 2, 3, 4, 5, 6, 7, 8},
            addedNumConst = {123, 456}
    )
    static int override3(int i) {
        BarOverride3 x = new BarOverride3();
        return x.choose(i + 123, i + 456);
    }
    static abstract class FooOverride3{
        int choose(int x, int y){
            return y;
        }
    }
    static class BarOverride3 extends FooOverride3{
        int choose(int x, int y){
            return x;
        }
    }


    @joptimize.Test(
            inputs = {1, 2, 3, 4, 5, 6, 7, 8},
            addedNumConst = {123}
    )
    static int deadLoopCounter(int i) {
        return deadLoopCounter0(i + 123, i + 456);
    }
    static int deadLoopCounter0(int x, int y) {
        int z = 1;
        int w = y;
        while(z < x){
            z *= 2;
            w *= 3;
        }

        return z;
    }

}
