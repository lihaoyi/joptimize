import mill._, mill.scalalib._

object joptimize extends ScalaModule{
  def scalaVersion = "2.12.8"
  def ivyDeps = Agg(
    ivy"org.ow2.asm:asm:6.1.1",
    ivy"org.ow2.asm:asm-tree:6.1.1",
    ivy"org.ow2.asm:asm-analysis:6.1.1",
    ivy"org.ow2.asm:asm-commons:6.1.1",
    ivy"org.ow2.asm:asm-util:6.1.1",
    ivy"com.lihaoyi::os-lib:0.2.5",
    ivy"com.lihaoyi::pprint:0.5.3"
  )
  object test extends Tests{
    def ivyDeps = Agg(ivy"com.lihaoyi::utest::0.6.6")
    def testFrameworks = Seq("utest.runner.Framework")
  }
}
