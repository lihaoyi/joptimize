package test.scalalib
object Library {


  @test.Test(inputs = Array(0, 2, 4))
  def arrayBuilderOfInt(a: Int): Array[Int] = {
    new collection.mutable.ArrayBuilder.ofInt().result()
  }

  @test.Test()
  def touchManifestFactory(): String = {
    val s = scala.reflect.ManifestFactory.toString
    s.substring(0, s.indexOf('@'))
  }

  @test.Test()
  def touchScalaPackage(): String = {
    scala.Predef.int2Integer(123).toString
  }
}
