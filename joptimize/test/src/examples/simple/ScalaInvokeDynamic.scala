package joptimize.examples.simple

object ScalaInvokeDynamic {
    @joptimize.Test(inputs = Array(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12))
    def hello(a: Int) = {
        val x = 1
        a + x
    }
    @joptimize.Test(inputs = Array(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12))
    def hello2(a: Int) = {
        val x = 1
        this.getClass.getName.length + a + x
    }
    @joptimize.Test(inputs = Array(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12))
    def lambda(a: Int) = {
        val x = 1
        val r1: Function0[Int] = () => a + x
        r1()
    }

    @joptimize.Test(inputs = Array(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12))
    def lambdaArg(a: Int) = {
        val x = 1
        val r1: Function1[Double, Int] = d => d.toInt + x
        r1(10.0 / a)
    }

    @joptimize.Test(inputs = Array(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12))
    def lambdaBoxed(a: Int)  = {
        val x = 1
        val r1: Function0[Integer] = () => a + x
        r1()
    }

    @joptimize.Test(inputs = Array(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12))
    def lambdaBoxedArgs(a: Int) = {
        val x = 1
        val r1: Function2[java.lang.Double, java.lang.Float, java.lang.Integer] =
            (d, f) => (d + f).toInt + x
        r1(10.0 / a, 5.0f / a)
    }

    @joptimize.Test(inputs = Array(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12))
    def concat(lhs: Int, rhs: Int): String = {
        return lhs + " sep " + rhs
    }
}