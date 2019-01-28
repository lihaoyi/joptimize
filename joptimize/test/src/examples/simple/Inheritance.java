package joptimize.examples.simple;

public class Inheritance {
    @joptimize.Test(inputs = {1, 2, 4, 8})
    public static int hello(int n) {
        Object b = new Object();
        return b.getClass().getName().length() + n;
    }

    @joptimize.Test(inputs = {1, 2, 4, 8})
    public static String strings(int n) {
        String s = "v";
        for (int i = 0; i < n; i++) {
            s = s.concat("o");
        }
        return s;
    }

    @joptimize.Test()
    public static String getSetField() {
        Car honda = new Honda();
        ((Honda) honda).cc++;
        return "";
    }

    @joptimize.Test(inputs = {1, 2, 4, 8})
    public static String implement(int n) {
        Baas b = new Sheep();
        return b.baa(n);
    }

    @joptimize.Test()
    public static String abstractClass() {
        Car toyota = new Toyota();
        return toyota.vroom();
    }

    @joptimize.Test()
    public static String shadowedInheritedGet() {
        Car honda = new Honda();
        return honda.vroom();
    }

    @joptimize.Test()
    public static String shadowedInheritedSet() {
        Car honda = new Honda();
        honda.rev();
        honda.cc++;
        ((Honda) honda).cc++;
        return "";
    }

    @joptimize.Test()
    public static String superMethod() {
        return new Toyota().superVStart();
    }

    @joptimize.Test()
    public static int staticInheritance() {
        int a = Parent.x;
        Child1.x = 100;
        return a + Child1.x + Child2.x;
    }

    @joptimize.Test()
    public static int staticInheritanceMethod() {
        return Child1.inherited() + // Calls the method inherited from parent
                Parent.overriden() + // Calls the method directly on parent
                Child1.overriden(); // Calls the method on child overriding the one from parent
    }
}

interface ParentInterface {
    public static int x = 30;
}

class Parent {
    public static int x = 10;

    public static int inherited() {
        return 0xcafebabe;
    }

    public static int overriden() {
        return 1337;
    }
}

class Child1 extends Parent {
    public static int get() {
        return x;
    }

    public static int overriden() {
        return 31337;
    }
}

class Cowc {
}

class Child2 extends Cowc implements ParentInterface {
    public static int get() {
        return x;
    }
}

class Sheep implements Baas {
    public String baa(int n) {
        String s = "b";
        for (int i = 0; i < n; i++) s = s.concat("a");
        return s;
    }
}

interface Baas {
    public String baa(int n);
}

class Toyota extends Car {
    public Toyota() {
        this.cc = 10;
    }

    public String vStart() {
        return "vr";
    }

    public String superVStart() {
        return super.vStart();
    }
}

class Honda extends Car {
    public int cc = 5;

    public String vStart() {
        return "v".concat(String.valueOf(cc)).concat("r").concat(String.valueOf(((Car) this).cc)).concat("r").concat(String.valueOf(super.cc));
    }
}

class Car {
    public int cc;

    public String vStart() {
        return "";
    }

    public void rev() {
        this.cc = this.cc + 1;
    }

    public String vroom() {
        String s = vStart();
        for (int i = 0; i < cc; i++) {
            s = s.concat("o");
        }
        return s;
    }
}