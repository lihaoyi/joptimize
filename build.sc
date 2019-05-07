import mill._
import mill.scalalib._
import mill.scalajslib._
import mill.scalajslib.api.ModuleKind

object joptimize extends ScalaModule{
  def moduleDeps = Seq(viewer.model)
  def scalaVersion = "2.12.8"
  def ivyDeps = Agg(
    ivy"org.ow2.asm:asm:7.0",
    ivy"org.ow2.asm:asm-tree:7.0",
    ivy"org.ow2.asm:asm-analysis:7.0",
    ivy"org.ow2.asm:asm-commons:7.0",
    ivy"org.ow2.asm:asm-util:7.0",
    ivy"com.lihaoyi::os-lib:0.2.5",
    ivy"com.lihaoyi::pprint:0.5.3"
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

    def unzippedScalaFolder = T{
      val zipFile = new java.util.zip.ZipFile(
        compileClasspath().find(_.path.toString.contains("scala-library")).get.path.toString
      )
      import collection.JavaConverters._
      for(x <- zipFile.entries.asScala){
        if (!x.isDirectory){
          os.write(
            T.ctx().dest / os.RelPath(x.getName),
            zipFile.getInputStream(x),
            createFolders = true
          )
        }
      }

      PathRef(T.ctx().dest)
    }
    def forkEnv = Map(
      "SCALA_FOLDER" -> unzippedScalaFolder().path.toString,
      "CLASSES_FOLDER" -> compile().classes.path.toString()
    )
    def scalacOptions = super.scalacOptions() ++ Seq("-Ydelambdafy:inline")
  }
}

object viewer extends ScalaModule{
  def moduleDeps = Seq(viewer.model)
  def scalaVersion = "2.12.8"
  def ivyDeps = Agg(
    ivy"com.lihaoyi::cask:0.2.0",
    ivy"com.lihaoyi::os-lib:0.2.5",
    ivy"com.lihaoyi::scalatags:0.6.7",
    ivy"guru.nidi:graphviz-java:0.8.3",
  )

  object model extends ScalaModule{
    def scalaVersion = "2.12.8"
    def ivyDeps = Agg(
      ivy"com.lihaoyi::upickle:0.7.1",
      ivy"com.lihaoyi::pprint:0.5.3"
    )
  }
}
