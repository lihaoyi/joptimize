import mill._, mill.scalalib._

object joptimize extends ScalaModule{
  def scalaVersion = "2.12.8"
  def ivyDeps = Agg(
    ivy"org.ow2.asm:asm:7.0",
    ivy"org.ow2.asm:asm-tree:7.0",
    ivy"org.ow2.asm:asm-analysis:7.0",
    ivy"org.ow2.asm:asm-commons:7.0",
    ivy"org.ow2.asm:asm-util:7.0",
    ivy"com.lihaoyi::os-lib:0.2.5",
    ivy"com.lihaoyi::pprint:0.5.3",
    ivy"guru.nidi:graphviz-java:0.8.3",
    ivy"com.lihaoyi::scalatags:0.6.7"
  )
  def compileIvyDeps = Agg(ivy"com.lihaoyi::acyclic:0.1.7")
  def scalacOptions = Seq("-P:acyclic:force")
  def scalacPluginIvyDeps = Agg(ivy"com.lihaoyi::acyclic:0.1.7")
  object test extends Tests{
    def ivyDeps = Agg(
      ivy"com.lihaoyi::utest::0.6.6",
      ivy"com.lihaoyi::ujson:0.7.1"
    )
    def testFrameworks = Seq("joptimize.UTestFramework")
  }
}
