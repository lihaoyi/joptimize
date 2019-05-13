package test.simple

object ScalaInvokeDynamic {
    @test.Test(inputs = Array(1, 2, 3))
    def hello(a: Int) = {
        val x = 1
        a + x
    }
    @test.Test(inputs = Array(1, 2, 3))
    def hello2(a: Int) = {
        val x = 1
        this.getClass.getName.length + a + x
    }
    @test.Test(inputs = Array(1, 2, 3))
    def lambda(a: Int) = {
        val x = 1
        val r1: Function0[Int] = () => a + x
        r1()
    }

    @test.Test(inputs = Array(1, 2, 3))
    def lambdaArg(a: Int) = {
        val x = 1
        val r1: Function1[Double, Int] = d => d.toInt + x
        r1(10.0 / a)
    }

    trait Func1[-T1, +R] {
        def apply(v1: T1): R
    }

    @test.Test(inputs = Array(1, 2, 3))
    def lambdaSpecialized(a: Int) = {
        val r1: Func1[Object, Int] = new Func1[Object, Int]{
            def apply(v1: Object) = v1.getClass.getName.length + a
        }
        r1(Integer.valueOf(123)) + r1("abc")
    }

    @test.Test(inputs = Array(1, 2, 3))
    def lambdaBoxed(a: Int)  = {
        val x = 1
        val r1: Function0[Integer] = () => Integer.valueOf(a + x)
        r1()
    }

    @test.Test(inputs = Array(1, 2, 3))
    def lambdaBoxedArgs(a: Int) = {
        val x = 1
        val r1: Function2[java.lang.Double, java.lang.Float, java.lang.Integer] =
            (d, f) => (d + f).toInt + x
        r1(10.0 / a, 5.0f / a)
    }

    @test.Test(inputs = Array(1, 2, 3, 4))
    def concat(lhs: Int, rhs: Int): String = {
        return lhs + " sep " + rhs
    }
}